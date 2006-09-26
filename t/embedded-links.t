#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 3;
use Text::MediawikiFormat;

my $wikitext = <<WIKI;

[[SuperLink|[[Description|Desc]] of the [[Link]]]]

WIKI

{
    my $htmltext = Text::MediawikiFormat::format ($wikitext, {}, {});
    is $htmltext,
       qq{<p>[[SuperLink|<a href='Description'>Desc</a> of the }
       . qq{<a href='Link'>Link</a>]]</p>\n},
       '...ignore embedded links by default';
}

{
    # Redefine the delimiters to something different.
    my %tags = (extended_link_delimiters => [qw{[[ ]]}],
		link => \&_make_html_link);

    my $htmltext = Text::MediawikiFormat::format ($wikitext, \%tags, {});
    is $htmltext,
       qq{<p><a href='SuperLink'><a href='Description'>Desc</a> of the }
       . qq{<a href='Link'>Link</a></a></p>\n},
       '...processing all embedded links';

    sub _make_html_link
    {
	my ($link) = @_;
	my ($href, $title) = split qr/\|/, $link, 2;
	$title ||= $href;
	return "<a href='$href'>$title</a>";
    }
}

TODO:
{
    # Art Henry's bug; but not sure it's really a bug
    local $TODO = "Unsupported MediaWiki features.";

    my %tags = (link => \&link_handler);

    # Or with the link handler overridden.
    my $htmltext = Text::MediawikiFormat::format ($wikitext, \%tags, {});
    is $htmltext,
       "<p>Desc of the </p>\n",
       '...and also work with a handler override.';

    sub link_handler
    {
	my ($link, $opts) = @_;
	($link, my $title) = split /\|/, $link, 2;
	$title ||= $link;
	return $title;
    }
}