#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 6;

use_ok( 'Text::MediawikiFormat' ) or exit;
ok( exists $Text::MediawikiFormat::tags{ blockorder },
	'TWF should have a blockorder entry in %tags' );

# isan ARRAY
isa_ok( $Text::MediawikiFormat::tags{ blockorder }, 'ARRAY', '... and we hope it' );

like( join(' ', @{ $Text::MediawikiFormat::tags{ blockorder } }),
	qr/ordered.+ordered.+code/,
	'... and code should come after ordered and unordered' );

my $wikitext =<<END_HERE;
	* first list item
	* second list item
	* list item with a WikiLink
END_HERE

my $htmltext = Text::MediawikiFormat::format( $wikitext );

like( $htmltext, qr!<li>first list item!,
	'lists should be able to start on the first line of text' );
like( $htmltext, qr!href="WikiLink"!,
	'list item content should be formatted' );
