package SOAP::Data::ComplexType;
use strict;
use warnings;
use Carp ();

our $VERSION = 0.01;

use constant OBJ_URI 	=> undef;
use constant OBJ_TYPE	=> undef;	#format: ns:type
use constant OBJ_FIELDS => {};		#format: name=>[type, uri, attr]

our $AUTOLOAD;
sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $data = shift;	#can be HASH ref or SOAP::SOM->result object
	my $obj_fields = shift;	#href: name=>[(scalar)type, (href)attr]; or name=>[[(scalar)type, href], (href)attr]; or name=>[[(scalar)type, [(scalar)type, href]], (href)attr]; ...
	my $self = { _sdb_obj => SOAP::Data::ComplexType::Builder->new(readable=>1) };
	bless($self, $class);
	$data = $self->_convert_object_to_hashref($data) if (ref($data) eq (split(/::/, $class))[-1]);	#kludge: to support SOAP::Lite's virtually non-existant support of complex datatypes
	$self->_parse_obj_fields($data, $obj_fields, undef);
	return $self;
}

sub _convert_object_to_hashref {	#recursive method
	my $self = shift;
	my $obj = shift;
	my $ret_href = {};
	return undef unless ref($obj);
	foreach my $key (keys %{$obj}) {
		if (ref($obj->{$key})) {	#recursion
			$ret_href->{$key} = $self->_convert_object_to_hashref($obj->{$key});
		}
		else {	#base case
			$ret_href->{$key} = $obj->{$key};
		}
	}
	return $ret_href;
}

sub _parse_obj_fields {	#recursive method
	my $self = shift;
	my $data = shift;
	my $obj_fields = shift;
	my $parent_hierarchy = shift;	#format: SOAP::Data::ComplexType::Builder->fullname()

	### validate parameters ###
	my $ret_href;
	unless (ref($data) eq 'HASH') {
		Carp::confess "Input data not expected ref type: HASH";
		return undef;
	}
	unless (ref($obj_fields) eq 'HASH' && scalar keys %{$obj_fields} > 0) {
		Carp::confess "Object field definitions invalid or undefined.";
		return undef;
	}

	### generate data structures ###
	foreach my $key (keys %{$data}) {
		my $key_regex = $key;
		if (grep(/^$key_regex$/, keys %{$obj_fields})) {
			unless (defined $self->{$key}) {
				if (ref($obj_fields->{$key}->[0]) eq 'ARRAY') {	#recursion case: complex subtype up to N levels deep
					my $ref = $obj_fields->{$key}->[0];
					my $obj = $self->{_sdb_obj}->add_elem(
						name		=> $key,
						value		=> undef,
						type		=> $ref->[0],
						uri			=> $obj_fields->{$key}->[1],
						attributes	=> $obj_fields->{$key}->[2],
						parent		=> defined $parent_hierarchy ? $self->{_sdb_obj}->get_elem($parent_hierarchy) : undef
					);
#warn "Added element $key\n";
					if (ref($data->{$key}) eq 'HASH') { $self->_parse_obj_fields($data->{$key}, $ref->[1], $obj->fullname()); }
					else { Carp::cluck "Warning: Expected hash ref value for key '$key', found scalar. Ignoring data value '$data->{$key}'" if defined $data->{$key}; }
				}
				else {	#base case
					$self->{_sdb_obj}->add_elem(
						name		=> $key,
						value		=> $data->{$key},
						type		=> $obj_fields->{$key}->[0],
						uri			=> $obj_fields->{$key}->[1],
						attributes	=> $obj_fields->{$key}->[2],
						parent		=> defined $parent_hierarchy ? $self->{_sdb_obj}->get_elem($parent_hierarchy) : undef
					);
#warn "Added element $key=$data->{$key}\n";
				}
			}
			else { Carp::cluck "Warning: Ignoring duplicate data key name '$key'"; }
		}
#		else { Carp::cluck "Warning: Ignoring invalid data key name '$key'"; }
	}
	return $ret_href;
}

sub DESTROY {}
sub CLONE {}

sub AUTOLOAD {
	my $self = shift;
	my $class = ref($self) || $self;
	my $name = $AUTOLOAD;
	my $value = shift;
	$name =~ s/.*://o;   # strip fully-qualified portion
	my $elem;
	Carp::confess "Can't access '$name' element object in class $class" unless $elem = $self->{_sdb_obj}->get_elem($name);
	return @{$elem->value($value)}[0];	#set value if param passed
}

sub as_soap_data { return shift->{_sdb_obj}->to_soap_data(); }
sub as_xml_data { return shift->{_sdb_obj}->serialise(); }

package SOAP::Data::ComplexType::Builder;
#adds type, uri field to Builder object

use SOAP::Data::Builder 0.8;
use vars qw(@ISA);
@ISA = qw(SOAP::Data::Builder);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = $class->SUPER::new(@_);
	return bless($self, $class);
}

sub add_elem {
	my ($self,%args) = @_;
	my $elem = SOAP::Data::ComplexType::Builder::Element->new(%args);
	if ( $args{parent} ) {
		my $parent = $args{parent};
		unless (ref $parent eq 'SOAP::Data::ComplexType::Builder::Element') {
			$parent = $self->get_elem($args{parent});
		}
		$parent->add_elem($elem);
	} else {
		push(@{$self->{elements}},$elem);
	}
	return $elem;
}

sub get_as_data {
	my ($self,$elem) = @_;
	my @values;
	foreach my $value ( @{$elem->value} ) {
		next unless ($value);
		if (ref $value) {
			push(@values,$self->get_as_data($value))
		} else {
			push(@values,$value);
		}
	}
	my @data = ();

	if (ref $values[0]) {
		$data[0] = \SOAP::Data->value( @values );
		} else {
			@data = @values;
		}
		if ($elem->{header}) {
			$data[0] = SOAP::Header->name($elem->{name} => $data[0])->attr($elem->attributes())->type($elem->{type} ? $elem->{type} : undef)->uri($elem->{uri} ? $elem->{uri} : undef);
		} else {
		if ($elem->{isMethod}) {
			@data = ( SOAP::Data->name($elem->{name})->attr($elem->attributes())->type($elem->{type} ? $elem->{type} : undef)->uri($elem->{uri} ? $elem->{uri} : undef) 
				=> SOAP::Data->value(@values)->type($elem->{type} ? $elem->{type} : undef)->uri($elem->{uri} ? $elem->{uri} : undef) );
		} else {
			$data[0] = SOAP::Data->name($elem->{name} => $data[0])->attr($elem->attributes())->type($elem->{type} ? $elem->{type} : undef)->uri($elem->{uri} ? $elem->{uri} : undef);
		}
	}
	return @data;
}

package SOAP::Data::ComplexType::Builder::Element;
#supports type and uri; correctly handles '0' data value

use SOAP::Data::Builder::Element;
use vars qw(@ISA);
@ISA = qw(SOAP::Data::Builder::Element);

sub new {
	my ($class,%args) = @_;
	my $self = {};
	bless ($self,ref $class || $class);
	foreach my $key (keys %args) {
		$self->{$key} = defined $args{$key} ? $args{$key} : undef;
	}
	if ($args{parent}) {
		$self->{fullname} = (ref $args{parent}) ? $args{parent}->{fullname}: "$args{parent}/$args{name}";
	}
	$self->{fullname} ||= $args{name};
	$self->{VALUE} = defined $args{value} ? [ $args{value} ] : [];
	return $self;
}

sub add_elem {
    my $self = shift;
    my $elem;
    if (ref $_[0] eq __PACKAGE__) {
		$elem = $_[0];
		push(@{$self->{VALUE}},$elem);
    } else {
		$elem = {};
		bless ($elem,ref $self);
		my %args = @_;
		foreach my $key (keys %args) {
			$elem->{$key} = defined $args{$key} ? $args{$key} : undef;
		}
		$elem->{fullname} = $self->{fullname}."/$args{name}";
		$elem->{VALUE} = defined $args{value} ? [ $args{value} ] : [];
		$elem->{TYPE} = [ $args{type} ];
		$elem->{URI} = [ $args{uri} ];
		push(@{$self->{VALUE}},$elem);
    }
    return $elem;
}

sub name {
	my $self = shift;
	my $value = shift;
	if (defined $value) {
		$self->{name} = $value;
	} else {
		$value = $self->{name};
	}
	return $value;
}

sub value {
	my $self = shift;
	my $value = shift;
	if ($value) {
		if (ref $value) {
			$self->{VALUE} = $value;
		} else {
			$self->{VALUE} = defined $value ? [$value] : [];
		}
	} else {
		$value = $self->{VALUE};
	}
	return $value;
}

sub get_as_data {
	my $self = shift;
	my @values;
	foreach my $value ( @{$self->{VALUE}} ) {
		if (ref $value) {
			push(@values,$value->get_as_data())
		} else {
			push(@values,$value);
		}
	}

	my @data = ();

	if (ref $values[0]) {
		$data[0] = \SOAP::Data->value( @values );
	} else {
		@data = @values;
	}

	if ($self->{header}) {
		$data[0] = SOAP::Header->name($self->{name} => $data[0])->attr($self->{attributes})->type($self->{type} ? $self->{type} : undef)->uri($self->{uri} ? $self->{uri} : undef);
	} else {
		if ($self->{isMethod}) {
			@data = ( SOAP::Data->name($self->{name})->attr($self->{attributes})->type($self->{type} ? $self->{type} : undef)->uri($self->{uri} ? $self->{uri} : undef) 
				=> SOAP::Data->value(@values)->type($self->{type} ? $self->{type} : undef)->uri($self->{uri} ? $self->{uri} : undef) );
		} else {
			$data[0] = SOAP::Data->name($self->{name} => $data[0])->attr($self->{attributes})->type($self->{type} ? $self->{type} : undef)->uri($self->{uri} ? $self->{uri} : undef);
		}
	}

	return @data;
}

1;

__END__
=head1 NAME

SOAP::Data::ComplexType - An easy interface for creating and implementing infinitely complex SOAP::Data objects

=head1 SYNOPSIS

	package My::SOAP::Data::ComplexType::Foo;
	use strict;
	use warnings;
	use SOAP::Data::ComplexType;
	use vars qw(@ISA);
	@ISA = qw(SOAP::Data::ComplexType);

	use constant OBJ_URI    => 'http://foo.bar.baz';
	use constant OBJ_TYPE   => 'ns1:myFoo';
	use constant OBJ_FIELDS => {
		field1              => ['string', undef, undef],
		field2              => ['int', undef, undef],
		field3              => ['xsd:dateTime', undef, undef]
	};

	sub new {
		my $proto = shift;
		my $class = ref($proto) || $proto;
		my $data = shift;
		my $obj_fields = shift;
		$obj_fields = defined $obj_fields && ref($obj_fields) eq 'HASH' ? {%{$obj_fields}, %{+OBJ_FIELDS}} : OBJ_FIELDS;
		my $self = $class->SUPER::new($data, $obj_fields);
		return bless($self, $class);
	}

	package My::SOAP::Data::ComplexType::Bar;
	use strict;
	use warnings;
	use SOAP::Data::ComplexType;
	use vars qw(@ISA);
	@ISA = qw(SOAP::Data::ComplexType);

	use constant OBJ_URI    => 'http://bar.baz.uri';
	use constant OBJ_TYPE   => 'ns1:myBar';
	use constant OBJ_FIELDS => {
		val1                => ['string', undef, undef],
		val2                => [
			[
				My::SOAP::Data::ComplexType::Foo::OBJ_TYPE,
				My::SOAP::Data::ComplexType::Foo::OBJ_FIELDS
			],
			My::SOAP::Data::ComplexType::Foo::OBJ_URI, undef
		]
	};

	sub new {
		my $proto = shift;
		my $class = ref($proto) || $proto;
		my $data = shift;
		my $obj_fields = shift;
		$obj_fields = defined $obj_fields && ref($obj_fields) eq 'HASH' ? {%{$obj_fields}, %{+OBJ_FIELDS}} : OBJ_FIELDS;
		my $self = $class->SUPER::new($data, $obj_fields);
		return bless($self, $class);
	}

	########################################################################
	package main;

	my $request_obj = My::SOAP::Data::ComplexType::Bar->new({
		val1    => 'sometext',
		val2    => {
			field1  => 'moretext',
			field2  => 12345,
			field3  => '2005-10-26T12:00:00.000Z'
		}
	});
	print $request_obj->as_xml_data;

	use SOAP::Lite;
	my $result = SOAP::Lite
			->uri($uri)
			->proxy($proxy)
			->somemethod(\SOAP::Data->value($request_obj->as_soap_data))
			->result;
			
	#assuming the method returns an object of type Foo...
	if (ref($result) eq 'Foo') {
		my $result_obj = My::SOAP::Data::ComplexType::Foo->new($result);
		print "$_=".$result_obj->$_."\n" foreach keys %{+My::SOAP::Data::ComplexType::Foo::OBJ_FIELDS};
	}

=head1 ABSTRACT

SOAP::Data::ComplexType defines a structured interface to implement classes that 
represent infinitely complex SOAP::Data objects.  Object instances can dynamically 
generate complex SOAP::Data structures or pure XML as needed.  Fields of an object 
may be easily accessed by making a method call with name of the field as the method, 
and field values can be changed after object construction by using the same method 
with one parameter.

Blessed objects returned by a SOAP::Lite method's SOAP::SOM->result may be
used to reconstitute the object back into an equivalent ComplexType, thus solving 
shortcomings of SOAP::Lite's handling of complex types and allowing users
to access their objects in a much more abstract and intuive way.  This is also
exceptionally useful for applications that need use SOAP result objects in future
SOAP calls.

=head1 DESCRIPTION

This module is intended to make it much easier to create complex SOAP::Data objects 
in an object-oriented class-structure, as users of SOAP::Lite must currently craft SOAP
data structures manually.  It uses L<SOAP::Data::Builder> internally to store and generate 
object data.

I hope this module will greatly improve productivity of any SOAP::Lite programmer, 
especially those that deal with many complex datatypes or work with SOAP apps that 
implement inheritance.

=head1 IMPLEMENTATION

=head2 Creating a SOAP ComplexType class

	TODO (see SYNOPSYS for example, for now)

=head2 Class Methods

	My::SOAP::Data::ComplexType::Example->new( HASH )

		Constructor.  Expects HASH ref (or reference to blessed SOAP::SOM->result object).

=head2 Object Methods

	$obj->as_soap_data

		Returns all data as a list of SOAP::Data objects.

	$obj->as_xml_data

		Returns all data formatted as an XML string.

	$obj->FIELDNAME( NEW_VALUE )

		Returns (or sets) the value of the given FIELDNAME field in your object. 
		NEW_VALUE is optional, and changes the current value of the object.

=head1 TODO

Changing the value of a field should also be able to support SOAP::Data::ComplexType
objects.  Currently, only scalar values can be used.

Support for more properties of a SOAP::Data object.  Currently only type, uri, attributes,
and value are supported.

A WSDL (and perhaps even an ASMX) parser may be included in the future to auto-generate 
ComplexType classes, thus eliminating nearly all the usual grunt effort of integrating a 
Perl application with complex applications running under modern SOAP services such as
Apache Axis or Microsoft .NET.

Add a test suite.

Complete this documentation :-)

=head1 CAVIATS

The OBJ_FIELD data structure may change in future versions to more cleanly support 
SOAP::Data parameters.  For now, I plan to keep it an array reference and simply append
on new SOAP::Data parameters as they are implemented.  Accessor methods may change as well,
as the current interface is a little weak--it only returns first matched occurance of an
element in the tree if there are multiple same-named elements.

=head1 BUGS

None known at this time. Bug reports and design suggestions are always welcome.

=head1 AUTHOR

Eric Rybski

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Eric Rybski, All Rights Reserved

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=head1 SEE ALSO

L<SOAP::Lite> L<SOAP::Data::Builder>

=cut
