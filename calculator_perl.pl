#!/usr/bin/perl
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);

my %variables;

while (1) {

    print "Enter an expression: ";

    my $input = <STDIN>;
    last unless defined $input;
    chomp $input;
    $input =~ s/^\s+|\s+$//g;
    next if $input eq '';

    if ($input =~ /^\s*(exit|quit)\s*$/i) {

        last;
    }

    elsif ($input =~ /=/) {

        my ($variable_name, $expression) = split /\s*=\s*/, $input, 2;
        $variable_name //= '';
        $expression //= '';

        unless ($variable_name =~ /^[a-zA-Z_]\w*$/) {

            next;
        }

        my %required_variables;
        my @variables_in_expression = ($expression =~ /([a-zA-Z_]\w*)/g);
        @required_variables{@variables_in_expression} = ();

        my $error;
        foreach my $var (keys %required_variables) {

            unless (exists $variables{$var}) {
                print "this is not a valid input: '$var'\n";
                $error = 1;
                last;
            }
        }
        next if $error;

        my $substituted_expression = $expression;
        my @sorted_variable_names = sort { length $b <=> length $a } keys %required_variables;

        foreach my $var (@sorted_variable_names) {

            my $value = $variables{$var};
            $substituted_expression =~ s/\b\Q$var\E\b/$value/g;
        }

        my $result;
        {
            local $@;
            $result = eval($substituted_expression);
            if ($@) {
                chomp $@;
                print "Error: $@\n";
                next;
            }
        }

        unless (looks_like_number($result)) {

            next;
        }

        $variables{$variable_name} = $result;
        print "$variable_name = $result\n";
    }

    else {

        my $expression = $input;

        my %required_variables;
        my @variables_in_expression = ($expression =~ /([a-zA-Z_]\w*)/g);
        @required_variables{@variables_in_expression} = ();

        my $error;
        foreach my $var (keys %required_variables) {

            unless (exists $variables{$var}) {
                print "this is not a valid input: '$var'\n";
                $error = 1;
                last;
            }
        }
        next if $error;

        my $substituted_expression = $expression;
        my @sorted_variable_names = sort { length $b <=> length $a } keys %required_variables;

        foreach my $var (@sorted_variable_names) {

            my $value = $variables{$var};
            $substituted_expression =~ s/\b\Q$var\E\b/$value/g;
        }

        my $result;
        {
            local $@;
            $result = eval($substituted_expression);

            if ($@) {

                chomp $@;
                print "Error: $@\n";
                next;
            }
        }

        print "$result\n";
    }
}
