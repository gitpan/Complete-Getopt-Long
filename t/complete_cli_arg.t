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
        comp_line0  => 'CMD -^',
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
        name        => 'option name with mentioned non-repeatable option',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --flag1 -^',
        result      => [qw/--bool --flag2 --float --int --no-bool
                           --nobool --str -F -S -f/],
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option 2',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -^  --flag1',
        result      => [qw/--bool --flag2 --float --int --no-bool
                           --nobool --str -F -S -f/],
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option 3 (alias)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --flag2 -^', # means -f is also mentioned
        result      => [qw/--bool --flag1 --float --int --no-bool
                           --nobool --str -F -S/],
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option 4 (alias)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD -f --f^', # means --flag1 is also mentioned
        result      => [qw/--flag1 --float/],
    );
    test_complete(
        name        => 'option name with mentioned non-repeatable option 5',
        args        => {getopt_spec=>{'--foo'=>sub{}, '--foot'=>sub{}}, },
        comp_line0  => 'CMD --foo -^',
        result      => [qw/--foot/],
    );
    my @foo;
    test_complete(
        name        => 'repetable option name 1 (assigned to arrayref)',
        args        => {getopt_spec=>{'foo=s'=>\@foo, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo 1 --bar 2 --^',
        result      => [qw/--foo/],
    );
    test_complete(
        name        => 'repetable option name 2 (desttype @)',
        args        => {getopt_spec=>{'foo=s@'=>sub{}, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo 1 --bar 2 --^',
        result      => [qw/--foo/],
    );
    test_complete(
        name        => 'repetable option name 3 (desttype %)',
        args        => {getopt_spec=>{'foo=s%'=>sub{}, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo 1 --bar 2 --^',
        result      => [qw/--foo/],
    );
    test_complete(
        name        => 'repetable option name 4 (incremental)',
        args        => {getopt_spec=>{'foo+'=>sub{}, 'bar=s'=>sub{}}, },
        comp_line0  => 'CMD --foo --bar 2 --^',
        result      => [qw/--foo/],
    );
    # even though there's completion routine, /^-/ indicates that user wants to
    # complete option name
    test_complete(
        name        => 'option name with completion routine',
        args        => {getopt_spec=>\%gospec,
                        completion=>sub { [qw/-x/] }},
        comp_line0  => 'CMD -^',
        result      => [qw/--bool --flag1 --flag2 --float --int --no-bool
                           --nobool --str -F -S -f/],
    );

    test_complete(
        name        => 'option value without completion routine',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --str ^',
        result      => [qw//],
    );
    test_complete(
        name        => 'option value without completion routine (2)',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --str=^',
        result      => [qw//],
    );
    test_complete(
        name        => 'option value for unknown option',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --foo=^',
        result      => [qw//],
    );
    test_complete(
        name        => 'option value for option that does not expect value',
        args        => {getopt_spec=>\%gospec, },
        comp_line0  => 'CMD --flag1=^',
        result      => [qw//],
    );
    test_complete(
        name        => 'option value with completion routine returning array',
        args        => {getopt_spec=>\%gospec,
                        completion=>sub { [qw/aa a b c/] }},
        comp_line0  => 'CMD --str ^',
        result      => [qw/a aa b c/],
    );

    # accepting hash is deprecated & undocumented but i think i'll support this
    # for a while
    test_complete(
        name        => 'option value with hash completion routine',
        args        => {getopt_spec=>\%gospec,
                        completion=>{ 'str|S=s' => [qw/aa a b c/] }},
        comp_line0  => 'CMD --str ^',
        result      => [qw/a aa b c/],
    );
    test_complete(
        name        => 'arg with hash completion routine',
        args        => {getopt_spec=>\%gospec,
                        completion=>{ '' => sub{[qw/aa a b c/]} }},
        comp_line0  => 'CMD --str 1 ^',
        result      => [qw/a aa b c/],
    );

    # XXX test option value with completion routine returning hash

    test_complete(
        name        => 'arg with completion routine returning array',
        args        => {getopt_spec=>\%gospec,
                        completion=>sub { [qw/aa a b c/] }},
        comp_line0  => 'CMD ^',
        result      => [qw/a aa b c/],
    );

    # XXX test arg with code completion returning hash

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
        $comp_line =~ s/\^//;

        require Complete::Bash;
        my ($words, $cword) = @{ Complete::Bash::parse_cmdline(
            $comp_line, $comp_point, '=') };
        shift @$words; $cword--; # strip command name

        require Complete::Getopt::Long;
        my $res = Complete::Getopt::Long::complete_cli_arg(
            words=>$words, cword=>$cword,
            completion=>sub{[]},
            %{$args{args}},
        );
        #use DD; dd { words=>$words, cword=>$cword, %{$args{args}} };
        is_deeply($res, $args{result}, "result") or diag explain($res);

        done_testing();
    };
}
