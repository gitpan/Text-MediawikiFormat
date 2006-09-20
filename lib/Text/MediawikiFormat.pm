package Text::MediawikiFormat;

use strict;

use Carp qw(carp confess croak);
use CGI qw(:standard);
use Scalar::Util qw(blessed);
use Text::MediawikiFormat::Blocks;
use URI;
use URI::Escape;

our $missing_html_packages;
BEGIN
{
    eval {require HTML::Parser};
    $missing_html_packages = $@;
    eval {require HTML::Tagset};
    $missing_html_packages .= $@;
}

use IO::File;
our $df = new IO::File (">>/tmp/wikiformat.log");

use vars qw( $VERSION %tags %opts %merge_matrix );
$VERSION = '0.01'; 
%tags    = (
    indent		=> qr/^(?:\t+|\s{4,})/,
    newline		=> '<br />',
    link		=> \&make_html_link,
    strong		=> sub {"<strong>$_[0]</strong>"},
    emphasized		=> sub {"<em>$_[0]</em>"},
    strong_tag		=> qr/'''(.+?)'''/,
    emphasized_tag	=> qr/''(.+?)''/,

    code		=> ['<pre><code>', "</code></pre>\n", '', "\n"],
    line		=> ['', "\n", '<hr />',  "\n"],
    paragraph		=> ['<p>', "</p>\n", '', "<br />\n", 1],
    paragraph_break	=> ['', '', '', "\n"],
    unordered		=> ["<ul>\n", "</ul>\n", '<li>', "</li>\n"],
    ordered		=> ["<ol>\n", "</ol>\n", 
	sub {qq|<li value="$_[2]">|, $_[0], "</li>\n"}],
    header		=> [ '', "\n",
	sub
	{
	    my $level = length $_[2];
	    return "<h$level>", format_line($_[3], @_[-2, -1]), "</h$level>\n"
	},],

    blocks		=> {
	ordered			=> qr/^([\dA-Za-z]+)\.\s*/,
	unordered		=> qr/^\*\s*/,
	code			=> qr/^(?:\t+|\s{4,})  /,
	header			=> qr/^(=+) (.+) \1/,
	paragraph		=> qr/^/,
	paragraph_break		=> qr/^\s*$/,
	line			=> qr/^-{4,}/,
    },

    indented		=> {map {$_ => 1} qw(ordered unordered)},
    nests		=> {map {$_ => 1} qw(ordered unordered)},
    nests_anywhere	=> {map {$_ => 1} qw(nowiki)},

    blockorder		=> [qw(header line ordered unordered code
			       paragraph_break paragraph)],
    extended_link_delimiters
			=> [qw([ ])],

    schemas		=> [qw(http https ftp mailto gopher)],

    unformatted_blocks
		    => [qw(code header nowiki pre)],

    allowed_tags	=> [#HTML
			    qw(b big blockquote br caption center cite code dd
			       div dl dt em font h1 h2 h3 h4 h5 h6 hr i li ol p
			       pre rb rp rt ruby s small strike strong sub sup
			       table td th tr tt u ul var),
			       # Mediawiki Specific
			       qw(nowiki),],
    allowed_attrs	=> [qw(title align lang dir width height bgcolor),
			    qw(clear), # BR
			    qw(noshade), # HR
			    qw(cite), # BLOCKQUOTE, Q
			    qw(size face color), # FONT
			    # For various lists, mostly deprecated but safe
			    qw(type start value compact),
			    # Tables
			    qw(summary width border frame rules cellspacing
			       cellpadding valign char charoff colgroup col
			       span abbr axis headers scope rowspan colspan),
			    qw(id class name style), # For CSS
			   ],

);
%opts = (
    implicit_links	=> 1,
);

sub process_args
{
	shift; # Class
	return as => shift if @_ == 1;
	return as => 'wikiformat', @_;
}

# Everything defaults to undef except implicit_links, which defaults to 1.
sub default_opts
{
	return {
		%opts,
		map {$_ => delete $_[1]->{$_}}
		    qw(prefix extended implicit_links absolute_links
		       process_html debug)
	       };
}

# Shamelessly ripped from Hash::Merge, which doesn't work in a threaded
# environment with two threads trying to use different merge matrices.
%merge_matrix = (
	SCALAR => {
                SCALAR => sub { return $_[0] },
                ARRAY  =>
			sub { # Need to be able to replace scalar with array
			      # for extended_link_delimiters (could be array
			      # or regex).
			      return $_[0]; },
                HASH   =>
			sub { confess "Attempt to replace hash with scalar"
				if defined $_[0];
			      return _clone ($_[1]); } },
        ARRAY => {
                SCALAR =>
			sub { # Need to be able to replace array with scalar
			      # for extended_link_delimiters (could be array
			      # or regex).
			      return _clone ($_[0]); },
                ARRAY  => sub { return _clone ($_[0]); },
                HASH   =>
			sub { confess "Attempt to replace hash with array" } },
        HASH => {
                SCALAR => sub { confess "Attempt to replace scalar with hash" },
                ARRAY  => sub { confess "Attempt to replace array with hash" },
                HASH   => sub { _merge_hashes( $_[0], $_[1] ) } }
);
# Return a copy of arrays and a deep copy of hashes.
sub _clone
{
  my ($obj) = @_;
  my $type;
  if (!defined $obj) {		# Perl 5.005 compatibility
    $type = 'SCALAR';
  } elsif (ref $obj eq 'HASH') { 
    $type = 'HASH';
  } elsif (ref $obj eq 'ARRAY') {
    $type = 'ARRAY';
  } else {
    $type = 'SCALAR';
  }

  return $obj if $type eq 'SCALAR';
  return [@$obj] if $type eq 'ARRAY';

  my %copy;
  foreach my $key (keys %$obj)
  {
    $copy{$key} = _clone ($obj->{$key});
  }
  return \%copy;
}
# This does a straight merge of hashes, delegating the merge-specific 
# work to 'merge'.
sub _merge_hashes
{
  my ( $left, $right ) = ( shift, shift );
  die "Arguments for _merge_hashes must be hash references" unless 
    UNIVERSAL::isa( $left, 'HASH' ) && UNIVERSAL::isa( $right, 'HASH' );
  
  my %newhash;
  foreach my $leftkey ( keys %$left ) {
    if ( exists $right->{ $leftkey } ) {
      $newhash{ $leftkey } = 
	merge_hash ($left->{ $leftkey }, $right->{ $leftkey });
    } else {
      $newhash{ $leftkey } = _clone ($left->{ $leftkey });
    }
  }
  foreach my $rightkey ( keys %$right ) { 
    $newhash{ $rightkey } = _clone ($right->{ $rightkey })
      if !exists $left->{ $rightkey };
  }
  return \%newhash;
}
sub merge_hash
{
  my ( $left, $right ) = ( shift, shift );
  
  # For the general use of this module, we want to create duplicates
  # of all data that is merged.  This behavior can be shut off, but 
  # can mess havoc if references are used heavily.
  
  my ($lefttype, $righttype);
  if (!defined $left) {		# Perl 5.005 compatibility
    $lefttype = 'SCALAR';
  } elsif (ref $left eq 'HASH') { 
    $lefttype = 'HASH';
  } elsif (ref $left eq 'ARRAY') {
    $lefttype = 'ARRAY';
  } else {
    $lefttype = 'SCALAR';
  }
  
  if (!defined $right) {		# Perl 5.005 compatibility
    $righttype = 'SCALAR';
  } elsif (ref $right eq 'HASH') { 
    $righttype = 'HASH';
  } elsif (ref $right eq 'ARRAY') {
    $righttype = 'ARRAY';
  } else {
    $righttype = 'SCALAR';
  }
  
  return $merge_matrix{$lefttype}->{$righttype} ($left, $right);
}	



sub _require_html_packages
{
    croak "$missing_html_packages\n"
	  . "HTML::Parser & HTML::Tagset is required for process_html\n"
	if $missing_html_packages;
}

sub import
{
    return unless @_ > 1;

    my $class   = shift;
    my %args    = $class->process_args( @_ );
    my $name    = delete $args{as};

    my $caller  = caller();
    my $iopts = $class->default_opts (\%args);
    my $itags = merge_hash (\%args, \%tags);

    _require_html_packages
	if $iopts->{process_html};

    # Could verify ITAGS here via check_blocks, but what if a user
    # wants to add a block to block_order that they intend to override
    # the implementation of with every call to format()?

    no strict 'refs';
    *{ $caller . "::" . $name } = sub
    {
	Text::MediawikiFormat::_format ($itags, $iopts, @_);
    }
}

sub format
{
    _format (\%tags, \%opts, @_);
}

sub _format
{
	my ($itags, $iopts, $text, $tags, $opts) = @_;

	# Overwriting the caller's hashes locally after merging its contents
	# is okay.
	$tags = merge_hash ($tags || {}, $itags);
	$opts = merge_hash ($opts || {}, $iopts);

	_require_html_packages
	    if $iopts->{process_html};

	# Always verify the blocks since the user may have slagged the
	# default hash on import.
	check_blocks( $tags );

	my @blocks =  find_blocks( $text,     $tags, $opts );
	@blocks    =  nest_blocks( \@blocks                 );
	return     process_blocks( \@blocks,  $tags, $opts );
}

sub check_blocks
{
    my $tags = shift;
    my %blocks = %{$tags->{blocks}};
    delete @blocks{@{$tags->{blockorder}}};

    carp "No order specified for blocks '"
	 . join (', ', keys %blocks)
	 . "'\n"
	if keys %blocks;
}



# This sub recognizes three states:
#
#   1.  undef
#       Normal wiki processing will be done on this line.
#
#   2.  html
#       Links and phrasal processing will be done, but formatting should be
#       ignored.
#
#   3.  nowiki
#       No further wiki processing should be done.
#
# Each state may override the lower ones if already set on a given line.
#
sub _append_processed_line
{
    my ($parser, $text, $state) = @_;
    my $lines = $parser->{processed_lines};

    $state ||= '';

    print $df "_append_processed_line ($state, ", $text, ")\n"
	if $parser->{opts}->{debug};

    my @newlines = split /(?<=\n)/, $text;
    if (@$lines && $lines->[-1]->[1] !~ /\n$/
	&& # State not changing from or to 'nowiki'
	   !($state ne $lines->[-1]->[0]
	     && ($state eq 'nowiki' || $lines->[-1]->[0] eq 'nowiki')))
    {
	$lines->[-1]->[1] .= shift @newlines;
	$lines->[-1]->[0] = $state if $state eq 'html';
	print $df "_append_processed_line: ", $lines->[-1]->[1]
	    if $parser->{opts}->{debug};
    }

    foreach my $line (@newlines)
    {
	$lines->[-1]->[2] = '1' if @$lines;
	push @$lines, [$state, $line];
	print $df "_append_processed_line: ", $line
	    if $parser->{opts}->{debug};
    }
    $lines->[-1]->[2] = '1'
	    if @$lines && $lines->[-1]->[1] =~ /\n$/;
}

sub _html_tag
{
    my ($parser, $type, $tagname, $orig, $attr) = @_;
    my $tags = $parser->{tags};

    unless (grep /^\Q$tagname\E$/, @{$tags->{allowed_tags}})
    {
	_append_processed_line $parser, CGI::escapeHTML $orig;
	return;
    }
    # Any $tagname must now be in the allowed list, including <nowiki>.

    my $tagstack = $parser->{tag_stack};

    # First, process end tags, since they can change our state.
    if ($type eq 'E' && @$tagstack && $tagstack->[-1] eq $tagname)
    {
	pop @$tagstack;
	my $newtag;

	if ($tagname eq 'nowiki')
	{
	    # The browser doesn't need to see the </nowiki> tag.
	    $newtag = '';
	}
	else
	{
	    $newtag = "</$tagname>";
	}

	# Can't close a state into <pre> or <nowiki>
	my $newstate;
	$newstate = 'html'
	    unless $HTML::Tagset::isPhraseMarkup{$tagname};

	_append_processed_line $parser, $newtag, $newstate;
	return;
    }

    if ($type eq 'E' && $HTML::Tagset::isPhraseMarkup{$tagname})
    {
	_append_processed_line $parser, "</$tagname>";
	return;
    }

    if ($type eq 'E'
	|| @$tagstack
	   && ($tagstack->[-1] eq 'pre' || $tagstack->[-1] eq 'nowiki'))
    {
	# We just tried to close a non-phrase that isn't on the
	# stack and/or the current state is <pre> or <nowiki>.
	my $newstate;
	if (@$tagstack)
	{
	    if ($tagstack->[-1] eq 'pre')
	    {
		$newstate = 'html';
	    }
	    elsif ($tagstack->[-1] eq 'nowiki')
	    {
		$newstate = 'nowiki';
	    }
	}

	_append_processed_line $parser, CGI::escapeHTML ($orig), $newstate;
	return;
    }


    ###
    ### $type must now eq 'S'.
    ###

    # The browser doesn't need to see the <nowiki> tag.
    if ($tagname eq 'nowiki')
    {
	push @$tagstack, $tagname;
	return;
    }

    # Strip disallowed attributes.
    my $newtag = "<$tagname";
    foreach (@{$tags->{allowed_attrs}})
    {
	    if (defined $attr->{$_})
	    {
		    $newtag .= " $_";
		    unless ($attr->{$_}
			    eq '__TEXT_MEDIAWIKIFORMAT_BOOL__')
		    {
			    # CGI::escapeHTML escapes single quotes.
			    $attr->{$_} = CGI::escapeHTML $attr->{$_};
			    $newtag .= "='" . $attr->{$_} . "'";
		    }
	    }
    }
    $newtag .= " /" if $HTML::Tagset::emptyElement{$tagname};
    $newtag .= ">";

    # If this isn't a block level element, there's no need to track nesting.
    if ($HTML::Tagset::isPhraseMarkup{$tagname}
	|| $HTML::Tagset::emptyElement{$tagname})
    {
	_append_processed_line $parser, $newtag;
	return;
    }

    # Some elements can close implicitly
    if (@$tagstack)
    {
	if ($tagname eq $tagstack->[-1]
	    && $HTML::Tagset::optionalEndTag{$tagname})
	{
	    pop @$tagstack;
	}
	elsif (!$HTML::Tagset::is_Possible_Strict_P_Content{$tagname})
	{
	    # Need to check more than the last item for paragraphs.
	    for (my $i = $#{$tagstack}; $i >= 0; $i--)
	    {
		my $checking = $tagstack->[$i];
		last if grep /^\Q$checking\E$/,
			@HTML::Tagset::p_closure_barriers;

		if ($tagstack->[$i] eq 'p')
		{
		    # pop 'em all.
		    splice @$tagstack, $i;
		    last;
		}
	    }
	}
    }

    # Could verify here that <li> and <table> sub-elements only appear where
    # they belong.

    # Push the new tag onto the stack.
    push @$tagstack, $tagname;

    _append_processed_line $parser, $newtag, 'html';
    return;
}

sub _html_comment
{
    my ($parser, $text) = @_;

    _append_processed_line $parser, $text, 'nowiki';
}

sub _html_text
{
    my ($parser, $dtext, $skipped_text, $is_cdata) = @_;
    my $tagstack = $parser->{tag_stack};
    my ($newtext, $newstate);

    print STDERR "Got skipped_text: `$skipped_text'\n"
	if $skipped_text;

    if (@$tagstack)
    {
	if ($tagstack->[-1] eq 'nowiki')
	{
	    $newstate = 'nowiki';
	}
	elsif ($tagstack->[-1] eq 'pre')
	{
	    $newstate = 'html';
	}
	elsif ($is_cdata && $HTML::Tagset::isCDATA_Parent{$tagstack->[-1]})
	{
	    $newtext = $dtext;
	}
    }

    $newtext = CGI::escapeHTML $dtext unless defined $newtext;

    _append_processed_line $parser, $newtext, $newstate;
}

sub _find_blocks_in_html
{
    my ($text, $tags, $opts) = @_;

    my $parser = HTML::Parser->new
	(start_h   => [\&_html_tag, 'self, "S", tagname, text, attr'],
	 end_h     => [\&_html_tag, 'self, "E", tagname, text'],
	 comment_h => [\&_html_comment, 'self, text'],
	 text_h    => [\&_html_text, 'self, dtext, skipped_text, is_cdata'],
	 marked_sections => 1,
	 boolean_attribute_value => '__TEXT_MEDIAWIKIFORMAT_BOOL__',
	);
    $parser->{opts} = $opts;
    $parser->{tags} = $tags;
    $parser->{processed_lines} = [];
    $parser->{tag_stack} = [];

    my @blocks;
    my @lines = split /\r?\n/, $text;
    for (my $i = 0; $i < @lines; $i++)
    {
	$parser->parse ($lines[$i]);
	$parser->parse ("\n");
	$parser->eof if $i == $#lines;

	# @{$parser->{processed_lines}} may be empty when tags are
	# still open.
	while (@{$parser->{processed_lines}}
	       && $parser->{processed_lines}->[0]->[2])
	{
	    my ($type, $dtext)
		= @{shift @{$parser->{processed_lines}}};

	    my $block;
	    if ($type)
	    {
		$block = start_block ($dtext, $tags, $opts, $type);
	    }
	    else
	    {
		chomp $dtext;
		$block = start_block ($dtext, $tags, $opts);
	    }
	    push @blocks, $block if $block;
	}
    }

    return @blocks;
}



sub find_blocks
{
	my ($text, $tags, $opts) = @_;
	my @blocks;

	if ($opts->{process_html})
	{
		@blocks = _find_blocks_in_html ($text, $tags, $opts);
	}
	else
	{
		# The original behavior.
		for my $line ( split(/\r?\n/, $text) )
		{
			my $block = start_block( $line, $tags, $opts );
			push @blocks, $block if $block;
		}
	}

	return @blocks;
}

sub start_block
{
    my ($text, $tags, $opts, $type) = @_;

    print $df "start_block (", join (":", @_), ")\n"
	if $opts->{debug};

    return new_block('end', level => 0) unless $text;
    return new_block ($type,
		      level => 0,
		      opts  => $opts,
		      text  => $text,
		      tags  => $tags,)
	    if $type;

    for my $block (@{$tags->{blockorder}})
    {
	my ($line, $level, $indentation)  = ($text, 0, '');

	if ($tags->{indented}{$block})
	{
	    ($level, $line, $indentation) = get_indentation ($tags, $line);
	    next unless $level;
	}

	print $df "testing `$line' as $block (", $tags->{blocks}{$block},
		  ")\n"
	    if $opts->{debug};
	my $marker_removed = length ($line =~ s/$tags->{blocks}{$block}//);

	next unless $marker_removed;

	return new_block ($block,
			  args  => [grep {defined} $1, $2, $3, $4, $5, $6, $7,
				    $8, $9],
			  level => $level || 0,
			  opts  => $opts,
			  text  => $line,
			  tags  => $tags,
			 );
    }
}

sub nest_blocks
{
    my $blocks    = shift;
    return unless @$blocks;

    my @processed = shift @$blocks;

    for my $block (@$blocks)
    {
	push @processed, $processed[-1]->nest( $block );
    }

    return @processed;
}

sub process_blocks
{
	my ($blocks, $tags, $opts) = @_;

	my @open;
	for my $block (@$blocks)
	{
		push @open, process_block( $block, $tags, $opts )
			unless $block->type() eq 'end';
	}

	return join('', @open);
}

sub process_block
{
    my ($block, $tags, $opts) = @_;
    my $type = $block->type();

    print $df "process_block ($type)\n"
	if $opts->{debug};

    my ($start, $end, $start_line, $end_line, $between);
    if ($tags->{$type})
    {
	($start, $end, $start_line, $end_line, $between) = @{$tags->{$type}};
    }
    else
    {
	($start, $end, $start_line, $end_line) = ('', '', '', '');
    }

    my @text = ();
    for my $line (grep (/^\Q$type\E$/, @{$tags->{unformatted_blocks}})
		  ? $block->text()
		  : $block->formatted_text())
    {
	if (blessed $line)
	{
		print "process_block: nested, diving\n"
			if $opts->{debug};
		my $prev_end = pop @text || ();
		push @text, process_block ($line, $tags, $opts), $prev_end;
		print "process_block: back\n"
			if $opts->{debug};
		next;
	}

	if ((ref ($start_line) || '') eq 'CODE')
	{
	    (my $start_line, $line, $end_line) = 
		$start_line->($line, $block->level(),
			      $block->shift_args(), $tags, $opts);
	    push @text, $start_line;
	}
	else
	{
	    push @text, $start_line;
	}
	push @text, $line, $end_line;
    }

    pop @text if $between;
    print $df "process_block: returning:",
	      join (":", $start||'', @text, $end||''), "\n"
	    if $opts->{debug};
    return join('', $start, @text, $end);
}

sub get_indentation
{
	my ($tags, $text) = @_;

	return 0, $text unless $text =~ s/($tags->{indent})//;
	return( length( $1 ) + 1, $text, $1 );
}

sub format_line
{
	my ($text, $tags, $opts) = @_;
	$opts ||= {};

	$text =~ s!$tags->{strong_tag}!$tags->{strong}->($1, $opts)!eg;
	$text =~ s!$tags->{emphasized_tag}!$tags->{emphasized}->($1, $opts)!eg;

	$text = find_extended_links( $text, $tags, $opts ) if $opts->{extended};

	$text =~ s|(?<!["/>=])\b((?:[A-Z][a-z0-9]\w*){2,})|
			  $tags->{link}->($1, $opts)|egx
			if !defined $opts->{implicit_links} or $opts->{implicit_links};

	return $text;
}

sub find_innermost_balanced_pair
{
	my ($text, $open, $close) = @_;

	my $start_pos             = rindex( $text, $open              );
	return if $start_pos == -1;

	my $end_pos               =  index( $text, $close, $start_pos );
	return if $end_pos   == -1;

	my $open_length           = length( $open );
	my $close_length          = length( $close );
	my $close_pos             = $end_pos + $close_length;
	my $enclosed_length       = $close_pos - $start_pos;

	my $enclosed_atom        = substr( $text, $start_pos, $enclosed_length );
	return substr( $enclosed_atom, $open_length, 0 - $close_length ),
	       substr( $text, 0, $start_pos ),
		   substr( $text, $close_pos );
}

sub find_extended_links
{
	my ($text, $tags, $opts) = @_;

    my $schemas = join('|', @{$tags->{schemas}});
    $text =~ s!(\s+)(($schemas):\S+)!$1 . $tags->{link}->($2, $opts)!egi
	    if $opts->{absolute_links};

    if (ref $tags->{extended_link_delimiters} eq "ARRAY")
    {
	my ($start, $end) = @{$tags->{extended_link_delimiters}};
	while (my @pieces = find_innermost_balanced_pair ($text, $start, $end))
	{
	    my ($tag, $before, $after) = map { defined $_ ? $_ : '' } @pieces;
	    my $extended               = $tags->{link}->( $tag, $opts, $tags )
					 || '';
	    $text                      = $before . $extended . $after;
	}
    }
    else
    {
	# Regexp
	$text =~ s/$tags->{extended_link_delimiters}/$tags->{link}->($1, $opts,
								     $tags)/ge;
    }

    return $text;
}

sub make_html_link
{
	my ($link, $opts)        = @_;
	$opts                  ||= {};

	($link, my $title)       = find_link_title( $link, $opts );
	($link, my $is_relative) = escape_link( $link, $opts );

	my $prefix               = ( defined $opts->{prefix} && $is_relative )
		? $opts->{prefix} : '';

	return qq|<a href="$prefix$link">$title</a>|;
}

sub escape_link
{
	my ($link, $opts) = @_;

	my $u = URI->new( $link );
	return $link if $u->scheme();

	# it's a relative link
	return( uri_escape( $link ), 1 );
}

sub find_link_title
{
	my ($link, $opts)  = @_;
	my $title;

	($link, $title)    = split(/\|/, $link, 2) if $opts->{extended};
	$title             = $link unless $title;

	return $link, $title;
}

'shamelessly adapted from the Jellybean project';

__END__

=head1 NAME

Text::MediawikiFormat - html-aware module for translating Wiki formatted text
                        into other formats

=head1 SYNOPSIS

	use Text::MediawikiFormat;
	my $html = Text::MediawikiFormat::format($raw);

=head1 DESCRIPTION

The original Wiki web site had a very simple interface to edit and to add
pages.  Its formatting rules are simple and easy to use.  They are also easy to
translate into other, more complicated markup languages with this module.  It
creates HTML by default, but can produce valid POD, DocBook, XML, or any other
format imaginable.

The most important function is C<format()>.  It is not exported by default.

=head2 format()

C<format()> takes one required argument, the text to convert, and returns the
converted text.  It allows two optional arguments.  The first is a reference to
a hash of tags.  Anything passed in here will override the default tag
behavior.  The second argument is a hash reference of options.  They are
currently:

=over 4

=item * prefix

The prefix of any links.  In HTML mode, this is the path to the Wiki.  The
actual linked item itself will be appended to the prefix.  This is useful to
create full URIs:

	{ prefix => 'http://example.com/wiki.pl?page=' }

=item * extended

A boolean flag, false by default, to use extended linking semantics.  This
comes from the Everything Engine (L<http:E<sol>E<sol>everydevel.comE<sol>>),
which marks links with square brackets.  An optional title may occur after the
link target, preceded by an open pipe.  These are valid extended links:

	[a valid link]
	[link|title]

Where the linking semantics of the destination format allow it, the result will
display the title instead of the URI.  In HTML terms, the title is the content
of an C<A> element (not the content of its C<HREF> attribute).

You can use delimiters other than single square brackets for marking extended
links by passing a value for C<extended_link_delimiters> in the C<%tags> hash
when calling C<format>.

=item * implicit_links

A boolean flag, true by default, to create links from StudlyCapsStringsNote
that if you disable this flag, you should probably enable the C<extended> one
also, or there will be no way of creating links in your documents.  To disable
it, use the pair:

	{ implicit_links => 0 }

=item * absolute_links

A boolean flag, false by default, which treats any links that are absolute URIs
(such as http://www.cpan.org/) specially. Any prefix will not apply and the
URIs aren't quoted. Use this in conjunction with the C<extended> option to
detect the link.

A link is any text that starts with a known schema followed by a colon and one
or more non-whitespace characters.  This is a distinct subset of what L<URI>
recognizes as a URI, but is a good first-order approximation.  If you need to
recognize more complex URIs, use the standard wiki formatting explained
earlier.

The recognized schemas are those defined in the C<schema> value in the C<%tags>
hash. The defaults are C<http>, C<https>, C<ftp>, C<mailto>, and C<gopher>.

=back

=head2 Wiki Format

Wiki formatting is very simple.  An item wrapped in three single quotes is
B<strong>.  An item wrapped in two single quotes is I<emphasized>.  Any word
with multiple CapitalLetters (e. g., StudlyCaps) will become a link.  Four or
more hyphen characters at the start of a line create a horizontal line.
Newlines turn into the appropriate tags.  Headers are matching equals signs
around the header text -- the more signs, the lesser the header.

Lists are indented text, by one tab or four spaces by default.  You may disable
indentation.  In unordered lists, where each item has its own bullet point,
each item needs a leading asterisk and space.  Ordered lists consist of items
marked with combination of one or more alphanumeric characters followed by a
period and an optional space.  Any indented text without either marking is
code, handled literally.  You can nest lists.

The following is valid Wiki formatting, with an extended link as marked.

	= my interesting text =

	ANormalLink
	[let the Sun shine|AnExtendedLink]

	== my interesting lists ==

	    * unordered one
	    * unordered two

	    1. ordered one
	    2. ordered two
			a. nested one
			b. nested two

	    code one
	    code two

	The first line of a normal paragraph.
	The second line of a normal paragraph.  Whee.

=head1 EXPORT

If you'd like to make your life more convenient, you can optionally import a
subroutine that already has default tags and options set up.  This is
especially handy if you use a prefix:

	use Text::MediawikiFormat prefix => 'http://www.example.com/';
	wikiformat( 'some text' );

Tags are interpreted as, well, tags, except for five special keys:

=over 4

=item * C<prefix>, interpreted as a link prefix

=item * C<extended>, interpreted as the extended link flag

=item * C<implicit_links>, interpreted as the flag to control implicit links

=item * C<absolute_links>, interpreted as the flag to control absolute links

=item * C<as>, interpreted as an alias for the imported function

=back

Use the C<as> flag to control the name by which your code calls the imported
functionFor example,

	use Text::MediawikiFormat as => 'formatTextInWikiStyle';
	formatTextInWikiStyle( 'some text' );

You might choose a better name, though.

The calling semantics are effectively the same as those of the format()
function.  Any additional tags or options to the imported function will
override the defaults.  This code:

	use Text::MediawikiFormat as => 'wf', extended => 0;
	wf( 'some text', {}, { extended => 1 });

enables extended links, though the default is to disable them.

Tony Bowden E<lt>tony@kasei.comE<gt> suggested this feature, but all
implementation blame rests solely with me.  Kate L Pugh
(E<lt>kake@earth.liE<gt>) pointed out that it didn't work, with tests.  It
works now.

=head1 GORY DETAILS

=head2 Tags

There are two types of Wiki markup: line items and blocks.  Blocks include
lists, which are made up of lines and can also contain other lists.

=head3 Line items

There are two classes of line items: simple tags, and tags that contain data.
The simple tags are C<newline> and C<line>.  The module inserts a newline tag
whenever it encounters a newline character (C<\n>).  It inserts a line tag
whenever four or more dash characters (C<---->) occur at the start of a line.
No whitespace is allowed.  These default to the E<lt>brE<gt> and E<lt>hrE<gt>
HTML tags, respectively.  To override either, simply pass tags such as:

	my $html = format($text, { newline => "\n" });

The three line items are more complex, and require subroutine references. This
category includes the C<strong> and C<emphasized> tags as well as C<link>s.
The first argument passed to the subref will be the data found in between the
marks.  The second argument is the $opts hash reference.  The default action
for a strong tag is equivalent to:

	my $html = format($text, { strong => sub { "<b>$_[0]</b>" } });

As of version 0.70, you can change the regular expressions used to find strong
and emphasized tags:

	%tags = (
		strong_tag     => qr/\*(.+?)\*/,
		emphasized_tag => qr|(?<!<)/(.+?)/|,
	);

	$wikitext = 'this is *strong*, /emphasized/, and */emphasized strong/*';
	$htmltext = Text::MediawikiFormat::format( $wikitext, \%tags, {} );

Be aware that using forward slashes to mark anything leads to the hairy regular
expression -- use something else.  B<This interface is experimental> and may
change if I find something better.  It's nice to be able to override those
tags, though.

Finally, there are C<extended_link_delimiters>, which allow you to use
delimiters other than single square brackets for marking extended links.  Pass
the tags as:

	my $html = format( $text, { extended_link_delimiters => [ '[[', ']]' ] });

This allows you to use double square brackets as UseMod supports:

	[[an extended link]]
	[[a titled extended link|title]]

=head3 Blocks

There are five default block types: C<paragraph>, C<header>, C<code>,
C<unordered>, and C<ordered>.  The parser usually finds these by indentation,
either one or more tabs or four or more whitespace characters.  (This does not
include newlines, however.)  Any line that does not fall in any of these three
categories is a C<paragraph>.

Code, unordered, and ordered blocks do not I<require> indentation, but the
parser uses it to control nesting in lists.  Be careful.  To mark a block as
requiring indentation, use the C<indented> tag, which contains a reference to a
hash:

	my $html = format($text, { 
		indented    => { map { $_ => 1 } qw( ordered unordered code )}
	});

Block entries in the tag hashes must contain array references.  The first two
items are the tags used at the start and end of the block.  The last items
contain the tags used at the start and end of each line.  Where there needs to
be more processing of individual lines, use a subref as the third item.  This
is how the module numbers ordered lines in HTML lists:

	my $html = format($text, { ordered => [ '<ol>', "</ol>\n",
		sub { qq|<li value="$_[2]">$_[0]</li>\n| } ] });

The first argument to these subrefs is the post-processed text of the line
itself.  (Processing removes the indentation and tokens used to mark this as a
list and checks the rest of the line for other line formattings.)  The second
argument is the indentation level.  The subsequent arguments are captured
variables in the regular expression used to find this list type.  The regexp
for ordered lists is:

	qr/^([\dA-Za-z]+)\.\s*/;

The module processes indentation first, if applicable, and stores the
indentation level (the length of the indentation removed).  The line must
contain one or more alphanumeric character followed by a single period and
optional whitespace to be an ordered list item.  The module saves the contents
of this last group, the value of the list item, and passes it to the subref as
the third argument.

Lists automatically start and end as necessary.

Because of the indentation issue, there is a specific blocks processing in a
specific order.  The C<blockorder> tag governs this order.  It contains a
reference to an array of the names of the appropriate blocks to process.  If
you add a block type, be sure to add an entry for it in C<blockorder>:

	my $html = format($text, {
		escaped       => [ '', '', '', '' ],
		blocks        => {
			invisible => qr!^--(.*?)--$!,
		},
		blockorder    =>
			[qw( header line ordered unordered code paragraph invisible )],
	});

=head3 Finding blocks

Text::MediawikiFormat uses regular expressions to find blocks.  These are in the
C<%tags> hash under the C<blocks> key.  To change the regular expression to
find code block items, use:

	my $html     =  format($wikitext, {
		blocks   => { 
			code => qr/^:\s+/,
		},
		indented => {
			code => 1,
		},
	);

This will require indentation and a colon to mark code lines.  A potential
shortcut is to use the C<indent> tag to match or to change the indentation
marker.  

B<Note>: if you want to mark a block type as non-indented, you B<cannot> use an
empty regex such as C<qr//>.  Use a mostly-empty, always-true regex such as
C<qr/^/> instead.

=head3 Finding Blocks in the Correct Order

As intrepid bug reporter Tom Hukins pointed out in CPAN RT bug #671, the order
in which Text::MediawikiFormat searches for blocks varies by platform and version of
Perl.  Because some block-finding regular expressions are more specific than
others, what you intend to be one type of block may turn into a different list
type.

If you're adding new block types, be aware of this.  The C<blockorder> entry in
C<%tags> exists to force Text::MediawikiFormat to apply its regexes from most
specific to least specific.  It contains an array reference.  By default, it
looks for ordered lists first, unordered lists second, and code references at
the end.

=head1 AUTHOR

Derek Price C<derek at ximbiot.com> is the current maintainer.  chromatic was
the original author of L<Text:WikiFormat>.  chromatic's original credits are
below:

chromatic, C<chromatic@wgz.org>, with much input from the Jellybean team
(including Jonathan Paulett).  Kate L Pugh has also provided several patches,
many failing tests, and is usually the driving force behind new features and
releases.  If you think this module is worth buying me a beer, she deserves at
least half of it.  

Alex Vandiver added a nice patch and tests for extended links.

Tony Bowden, Tom Hukins, and Andy H. all suggested useful features that are now
implemented.  

Sam Vilain, Chris Winters, Paul Schmidt, and Art Henry have all found and
reported silly bugs.

Blame me for the implementation.

=head1 BUGS

The link checker in C<format_line()> may fail to detect existing links that do
not follow HTML, XML, or SGML style.  They may die with some SGML styles too.
I<Sic transit gloria mundi>.

=head1 TODO

=over 4

=item * Find a nicer way to mark list as having unformatted lines

=item * Optimize C<format_line()> to work on a list of lines

=item * Handle nested C<strong> and C<emphasized> markings better

=back

=head1 OTHER MODULES

Brian "Ingy" Ingerson's L<CGI::Kwiki> has a fairly nice parser.

John McNamara's L<Pod::Simple::Wiki> looks like a good project.

Matt Sergeant keeps threatening to write a nice SAX-throwing Wiki formatter.

=head1 COPYRIGHT

Copyright (c) 2006 Derek R. Price.  All rights reserved.
Copyright (c) 2002 - 2006, chromatic.  All rights reserved.

This module is distributed under the same terms as Perl itself.
