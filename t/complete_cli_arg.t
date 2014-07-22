#!perl

use 5.010;
use strict;
use warnings;

use Test::More 0.98;

subtest basics => sub {
    my %gospec = (
        'flag1'       => sub{},
        'flag2|f'     => sub{},
        'bool!'       => sub{},
        'int=i'       => sub{},
        'float|F=f'   => sub{},
        'str|S=s'     => sub{},
    );

    test_complete(
        name        => 'option name',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD ^',
        result      => [qw/--bool --flag1 --flag2 --float --int --no-bool
                           --nobool --str -F -S -f/],
    );
    test_complete(
        name        => 'option name (2)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --f^',
        result      => [qw/--flag1 --flag2 --float/],
    );
    test_complete(
        name        => 'option name (3)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --str^',
        result      => [qw/--str/],
    );
    test_complete(
        name        => 'option value without completion',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --str ^',
        result      => [qw//],
    );
    test_complete(
        name        => 'option value with array completion',
        args        => {getopt_spec=>\%gospec,
                        completion=>{'str|S=s'=>[qw/aa a b c/]}},
        comp_line0  => 'CMD --str ^',
        result      => [qw/a aa b c/],
    );
    test_complete(
        name        => 'option value with array completion (2)',
        args        => {getopt_spec=>\%gospec,
                        completion=>{'str|S=s'=>[qw/aa a b c/]}},
        comp_line0  => 'CMD --str a^',
        result      => [qw/a aa/],
    );
    test_complete(
        name        => 'option value with code completion returning array',
        args        => {getopt_spec=>\%gospec,
                        completion=>{'str|S=s'=>sub { [qw/aa a b c/] }}},
        comp_line0  => 'CMD --str ^',
        result      => [qw/a aa b c/],
    );

    # XXX test option value with code completion returning hash

    test_complete(
        name        => 'option name + arg',
        args        => {getopt_spec=>\%gospec,
                        completion=>{''=>sub { [qw/aa a b c/] }}},
        comp_line0  => 'CMD ^',
        result      => [qw/--bool --flag1 --flag2 --float --int --no-bool
                           --nobool --str -F -S -f a aa b c/],
    );
    test_complete(
        name        => 'option name + arg (2)',
        args        => {getopt_spec=>\%gospec,
                        completion=>{''=>sub { [qw/-x/] }}},
        comp_line0  => 'CMD -^',
        result      => [qw/--bool --flag1 --flag2 --float --int --no-bool
                           --nobool --str -F -S -f -x/],
    );

    # XXX test arg with code completion returning hash
    # XXX test option name + arg with code completion returning hash

    # XXX test optional value
};

DONE_TESTING:
done_testing;

sub test_complete {
    my (%args) = @_;

    subtest +($args{name} // $args{comp_line0}) => sub {

        # $args{comp_line0} contains comp_line with '^' indicating where
        # comp_point should be, the caret will be stripped. this is more
        # convenient than counting comp_point manually.
        my $comp_line  = $args{comp_line0};
        defined ($comp_line) or die "BUG: comp_line0 not defined";
        my $comp_point = index($comp_line, '^');
        $comp_point >= 0 or
            die "BUG: comp_line0 should contain ^ to indicate where ".
                "comp_point is";
        $comp_point =~ s/\^//;

        require Complete::Bash;
        my ($words, $cword) = @{ Complete::Bash::parse_cmdline(
            $comp_line, $comp_point) };

        require Complete::Getopt::Long;
        my $res = Complete::Getopt::Long::complete_cli_arg(
            words=>$words, cword=>$cword, %{$args{args}},
        );
        is_deeply($res, $args{result}, "result") or diag explain($res);

        done_testing();
    };
}
