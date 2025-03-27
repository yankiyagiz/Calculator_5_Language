with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Containers; use Ada.Containers;  -- Added to resolve Count_Type visibility

procedure Calculator is
    Syntax_Error : exception;
    Undefined_Variable : exception;
    Division_By_Zero : exception;

    type Token_Kind is (
        T_Number,
        T_Identifier,
        T_Plus,
        T_Minus,
        T_Multiply,
        T_Divide,
        T_LParen,
        T_RParen,
        T_Assign,
        T_EOL
    );

    type Token_Type is record
        Kind : Token_Kind;
        Lexeme : Unbounded_String;
    end record;

    package Token_Vectors is new Ada.Containers.Vectors(
        Index_Type => Natural,
        Element_Type => Token_Type
    );
    use Token_Vectors;

    package Variable_Map is new Ada.Containers.Indefinite_Hashed_Maps(
        Key_Type => String,
        Element_Type => Float,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "="
    );
    use Variable_Map;

    Variables : Variable_Map.Map;

    procedure Tokenize(Input : String; Tokens : in out Token_Vectors.Vector) is
        Pos : Integer := Input'First;
    begin
    
        Tokens.Clear;
        
        while Pos <= Input'Last loop
        
            declare
                C : Character := Input(Pos);
            begin
            
                if C = ' ' or C = Character'Val(9) then -- ASCII.HT
                    Pos := Pos + 1;
                    
                elsif (C >= 'a' and C <= 'z') or (C >= 'A' and C <= 'Z') then
                    declare
                        Start : Integer := Pos;
                    begin
                    
                        while Pos <= Input'Last and then
                        
                              ((Input(Pos) >= 'a' and Input(Pos) <= 'z') or
                               (Input(Pos) >= 'A' and Input(Pos) <= 'Z') or
                               (Input(Pos) >= '0' and Input(Pos) <= '9')) loop
                            Pos := Pos + 1;
                        end loop;
                        
                        Tokens.Append((Kind => T_Identifier, Lexeme => To_Unbounded_String(Input(Start..Pos-1))));
                    end;
                    
                elsif (C >= '0' and C <= '9') or C = '.' then
                
                    declare
                        Start : Integer := Pos;
                        Has_Dot : Boolean := False;
                    begin
                    
                        while Pos <= Input'Last loop
                        
                            if Input(Pos) = '.' then
                            
                                if Has_Dot then
                                
                                    Raise_Exception(Syntax_Error'Identity, "Invalid number with multiple dots");
                                end if;
                                
                                Has_Dot := True;
                                Pos := Pos + 1;
                                
                            elsif Input(Pos) >= '0' and Input(Pos) <= '9' then
                                Pos := Pos + 1;
                                
                            else
                                exit;
                            end if;
                        end loop;
                        
                        if Pos = Start then
                        
                            Raise_Exception(Syntax_Error'Identity, "invalid number");
                        end if;
                        
                        Tokens.Append((Kind => T_Number, Lexeme => To_Unbounded_String(Input(Start..Pos-1))));
                    exception
                    
                        when others =>
                            Raise_Exception(Syntax_Error'Identity, "invalid number");
                    end;
                else
                    case C is
                        when '+' =>
                            Tokens.Append((Kind => T_Plus, Lexeme => To_Unbounded_String("+")));
                            Pos := Pos + 1;
                            
                        when '-' =>
                            Tokens.Append((Kind => T_Minus, Lexeme => To_Unbounded_String("-")));
                            Pos := Pos + 1;
                            
                        when '*' =>
                            Tokens.Append((Kind => T_Multiply, Lexeme => To_Unbounded_String("*")));
                            Pos := Pos + 1;
                            
                        when '/' =>
                            Tokens.Append((Kind => T_Divide, Lexeme => To_Unbounded_String("/")));
                            Pos := Pos + 1;
                            
                        when '(' =>
                            Tokens.Append((Kind => T_LParen, Lexeme => To_Unbounded_String("(")));
                            Pos := Pos + 1;
                            
                        when ')' =>
                            Tokens.Append((Kind => T_RParen, Lexeme => To_Unbounded_String(")")));
                            Pos := Pos + 1;
                            
                        when '=' =>
                            Tokens.Append((Kind => T_Assign, Lexeme => To_Unbounded_String("=")));
                            Pos := Pos + 1;
                            
                        when others =>
                            Raise_Exception(Syntax_Error'Identity, "invalid character: '" & C & "'");
                    end case;
                end if;
            end;
        end loop;
        
        Tokens.Append((Kind => T_EOL, Lexeme => To_Unbounded_String("")));
    exception
    
        when others =>
            Tokens.Clear;
            raise;
            
    end Tokenize;

    function Parse_Expression(Tokens : Token_Vectors.Vector; Position : in out Natural) return Float;
    function Parse_Term(Tokens : Token_Vectors.Vector; Position : in out Natural) return Float;
    function Parse_Factor(Tokens : Token_Vectors.Vector; Position : in out Natural) return Float;

    function Parse_Expression(Tokens : Token_Vectors.Vector; Position : in out Natural) return Float is
    
        Result : Float := Parse_Term(Tokens, Position);
        
    begin
    
        while Position < Tokens.Last_Index loop
        
            case Tokens(Position).Kind is
            
                when T_Plus =>
                    Position := Position + 1;
                    Result := Result + Parse_Term(Tokens, Position);
                    
                when T_Minus =>
                    Position := Position + 1;
                    Result := Result - Parse_Term(Tokens, Position);
                    
                when others =>
                    exit;
                    
            end case;
            
        end loop;
        
        return Result;
    exception
    
        when others =>
            raise;
    end Parse_Expression;

    function Parse_Term(Tokens : Token_Vectors.Vector; Position : in out Natural) return Float is
    
        Result : Float := Parse_Factor(Tokens, Position);
        
    begin
        while Position < Tokens.Last_Index loop
        
            case Tokens(Position).Kind is
            
                when T_Multiply =>
                    Position := Position + 1;
                    Result := Result * Parse_Factor(Tokens, Position);
                    
                when T_Divide =>
                    Position := Position + 1;
                    
                    declare
                        Divisor : constant Float := Parse_Factor(Tokens, Position);
                    begin
                    
                        if Divisor = 0.0 then
                            Raise_Exception(Division_By_Zero'Identity, "Division by zero");
                        end if;
                        Result := Result / Divisor;
                    end;
                    
                when others =>
                    exit;
                    
            end case;
            
        end loop;
        
        return Result;
        
    exception
        when others =>
            raise;
            
    end Parse_Term;

    function Parse_Factor(Tokens : Token_Vectors.Vector; Position : in out Natural) return Float is
    
        Token : Token_Type := Tokens(Position);
        
    begin
    
        case Token.Kind is
        
            when T_Number =>
            
                declare
                    Value : Float;
                begin
                    Value := Float'Value(To_String(Token.Lexeme));
                    Position := Position + 1;
                    return Value;
                    
                exception
                    when others =>
                        Raise_Exception(Syntax_Error'Identity, "invalid number: " & To_String(Token.Lexeme));
                end;
                
                
            when T_Identifier =>
            
                declare
                    Var_Name : constant String := To_String(Token.Lexeme);
                begin
                    if Variables.Contains(Var_Name) then
                        Position := Position + 1;
                        return Variables(Var_Name);
                    else
                        Raise_Exception(Undefined_Variable'Identity, "Variable '" & Var_Name & "' not defined");
                    end if;
                end;
                
                
            when T_LParen =>
            
                Position := Position + 1;
                declare
                
                    Expr_Value : constant Float := Parse_Expression(Tokens, Position);
                begin
                    if Position > Tokens.Last_Index or else Tokens(Position).Kind /= T_RParen then
                        Raise_Exception(Syntax_Error'Identity, "mismatched parenthesis");
                    end if;
                    
                    Position := Position + 1;
                    return Expr_Value;
                end;
                
                
            when T_Minus =>
            
                Position := Position + 1;
                return -Parse_Factor(Tokens, Position);
                
            when others =>
            
                Raise_Exception(Syntax_Error'Identity, "unidentified token: " & Token_Kind'Image(Token.Kind));
                
        end case;
        
    end Parse_Factor;

    procedure Handle_Assignment(Tokens : Token_Vectors.Vector) is
    
        Position : Natural := 0;
        Var_Name : Unbounded_String;
        Value : Float;
        
    begin
        if Tokens(0).Kind /= T_Identifier then
            Raise_Exception(Syntax_Error'Identity, "not a variable");
        end if;
        
        Var_Name := Tokens(0).Lexeme;
        if Tokens(1).Kind /= T_Assign then
            Raise_Exception(Syntax_Error'Identity, "Expected '=' in assignment");
        end if;
        
        Position := 2;
        Value := Parse_Expression(Tokens, Position);
        
        if Position /= Tokens.Last_Index or else Tokens(Position).Kind /= T_EOL then
            Raise_Exception(Syntax_Error'Identity, "unidentified token");
        end if;
        
        Variables.Include(To_String(Var_Name), Value);
    end Handle_Assignment;

begin

    loop
    
        Put("Enter expression: ");
        exit when End_Of_File;
        
        declare
            Line : constant String := Get_Line;
            Tokens : Token_Vectors.Vector;
            
        begin
            Tokenize(Line, Tokens);
            
            if not Tokens.Is_Empty then
            
                if Tokens(0).Kind = T_Identifier and then Tokens.Length >= 2 and then Tokens(1).Kind = T_Assign then
                    Handle_Assignment(Tokens);
                    
                else
                    declare
                        Position : Natural := 0;
                        Result : Float;
                    begin
                        Result := Parse_Expression(Tokens, Position);
                        if Position /= Tokens.Last_Index then
                            Raise_Exception(Syntax_Error'Identity, "unidentified token");
                        end if;
                        Ada.Float_Text_IO.Put(Result, Fore => 0, Aft => 6, Exp => 0);
                        New_Line;
                    end;
                end if;
                
            end if;
            
        exception
        
            when Error: Syntax_Error | Undefined_Variable | Division_By_Zero =>
                Put_Line(Exception_Message(Error));
            when Error: others =>
                Put_Line("error: " & Exception_Information(Error));
        end;
    end loop;
end Calculator;