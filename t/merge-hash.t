#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 8;
use_ok( 'Text::MediawikiFormat' ) or exit;

my $full       = { foo => { bar => 'baz' } };
my $empty      = {};
my $nonempty   = { foo => { a => 'b' } };
my $full_flat  = { a => 'b' };
my $empty_flat = {};
my $zero       = { foo => 0, bar => { baz => 0 } };

$nonempty = Text::MediawikiFormat::merge_hash( $full, $nonempty );
is_deeply( $nonempty, { foo => { a => 'b', bar => 'baz' } },
	"merge should work when all keys in from exist in to" );
$full->{foo}->{bar} = 'boo';
is_deeply( $nonempty, { foo => { a => 'b', bar => 'baz' } },
	"merge should copy subhashes" );

$empty_flat = Text::MediawikiFormat::merge_hash( $full_flat, $empty_flat );
is_deeply( $empty_flat, $full_flat,
	'... in flat case when keys exist in from but not in to' );

$empty = Text::MediawikiFormat::merge_hash( $full, $empty );
is_deeply( $empty, $full,
	'... in non-flat case when keys exist in but not in to' );

$empty = {};
$empty = Text::MediawikiFormat::merge_hash( $zero, $empty );
is_deeply( $empty, $zero, '... and when value is zero but defined' );

my $regexer = { a => "regex" };
my $arrayer = { a => ["X", "Y", "Z"] };
my $merged;
$merged = Text::MediawikiFormat::merge_hash( $regexer, $arrayer );
is_deeply( $merged, { a => "regex" }, "regexes should replace arrays" );
$merged = Text::MediawikiFormat::merge_hash( $arrayer, $regexer );
is_deeply( $merged, { a => ["X", "Y", "Z"] }, "...and vice versa" );
