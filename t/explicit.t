#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 7;
use Text::MediawikiFormat;

my $wikitext =<<WIKI;

[Ordinary extended link]

[http://nowhere.com|explicit URI]

[[Usemod extended link]]

WIKI

my $htmltext = Text::MediawikiFormat::format($wikitext, {}, { extended => 1 } );
like( $htmltext, qr!Ordinary extended link</a>!m,
	'extended links rendered correctly with default delimiters' );
like( $htmltext, qr!"http://nowhere\.com">explicit URI</a>!m,
	'explicit URIs rendered correctly with default delimiters' );

# Redefine the delimiters to the same thing again.
my %tags = (
	extended_link_delimiters => [ '[', ']' ]
);

$htmltext = Text::MediawikiFormat::format($wikitext, \%tags, { extended => 1 } );
like( $htmltext, qr!Ordinary extended link</a>!m,
	'...and again when delimiter redefined to the same thing' );

# Redefine the delimiters to something different.
%tags = (
	extended_link_delimiters => [ '[[', ']]' ]
);

$htmltext = Text::MediawikiFormat::format($wikitext, \%tags, { extended => 1 } );
unlike( $htmltext, qr!Ordinary extended link</a>!m,
	'old-style extended links not recognised when delimiter overridden' );

like( $htmltext, qr!Usemod extended link</a>[^\]]!m,
	'...and new delimiters recognised' );



##
## Mediawiki thinks [[...]] is a wiki link and [...] is a standard URI href.
##

# Turn [[Wiki Link|Title]] or [URI Title] into links.
sub _make_link
{
    my ($tag, $opts) = @_;

    my ($href, $title);
    if ($tag =~ /^\[(#?)([^|]*)(?:(|)(.*))?\]$/)
    {
	# Wiki link
	$href = $opts->{prefix} unless $1;
	$href .= $1 . $2;
	# Would normally URI::Escape::uri_escape $2 there, but it's probably
	# not worth importing that for tests.
	if ($3)
	{
	    # Title specified explicitly.
	    if (length $4)
	    {
		$title = $4;
	    }
	    else
	    {
		# An empty title asks Mediawiki to strip any parens off the end
		# of the node name.
		$2 =~ /^([^(]*)(?:\s*\()?/;
		$title = $1;
	    }
	}
	else
	{
	    # Title defaults to the node name.
	    $title = $2;
	}
    }
    else
    {
	# URI
	$tag =~ /^(\S*)(?:(\s+)(.*))?$/;
	$href = $1;
	if ($2)
	{
	    $title = $3;
	}
	else
	{
	    $title = ++$opts->{_uri_refs};
	}
	$href =~ s/'/%27/g;
    }

    return "<a href='$href'>$title</a>";
}

%tags = (
	extended_link_delimiters => qr/\[(\[[^][]*\]|[^][]*)\]/,
	link => \&_make_link,
);

$htmltext = Text::MediawikiFormat::format( $wikitext, \%tags,
					   { extended => 1,
					     implicit_links => 0 } );
like( $htmltext, qr!'Ordinary'>extended link</a>!m, 'mediawiki style href' );
like( $htmltext, qr!Usemod extended link</a>!m,
      'mediawiki style extended link' );

