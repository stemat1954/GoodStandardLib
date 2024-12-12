#include "preface.h"
#include "good_token.h"
#include "source_guid.h"
#include "good_name.h"
#include "good_entity.h"


string1 good_entity::v_get_description () const
{
	string_stream1 ss;	ss << this->get_descriptor() << " ";

	if ( this->name.is_pending() )
		ss << "(anon)";
	else if ( this->name.is_adhoc() )
		ss << this->name.use_key();
	else
		ss << this->name.get_sid();

	if ( !this->guid.is_void() ) ss << "/" << this->guid.get_page_name() << this->guid.get_spot();

	ss << " ";
	descriptive::write_address( ss, ( void* )this );

	if ( this->name.is_adhoc() ) ss << " = " << this->name.use_comment();

	return ss.str();
}
