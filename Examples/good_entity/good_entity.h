#pragma once


enum class permission_t // object access, ordered
{
	pending = 0,	// undefined
	eval	= 1,	// evaluate
	upd		= 2,	// update
	init	= 3		// initiate
};


enum class visibility_t  // entity visibility, ordered
{
	pending   = 0,  //undefined
	internal  = 1,	
	fraternal = 2,	
	external  = 3	
};


enum class opcode_t  // review opcodes
{
	adhoc_scan,	
	adhoc_copy
};


class page_reviewer;  // forward

class good_entity;  // forward


using spe_vector = vector< sp<good_entity> >;

using spe_map = map< good_entity*, sp<good_entity> >;
/*
	Note that the key pointer in an spe_map is a "raw" 
	pointer	because standard operator<() (defined below)
	sorts by entity name, which doesn't work for anon 
	entities.

	It's generally safe to use the raw pointer as a key 
	because it doesn't participate in ownership of the
	entity.
*/



class good_entity: public descriptive
/*
	A good_entity is a base type for all Good entities:

		good_type
			good_enum_type 
		good_nom_type
		good_procedure
			good_subroutine
			good_method
				good_abstract_method
		good_coroutine
		good_object
			good_literal
			good_enumerator
			good_analog
		good_proxy
		good_operation
		good_blank

	In general, an entity has a name and guid. 	A name is
	usually assigned by the application and it selects an
	entity in it's domain of definition, while a guid 
	uniquely identifies it throughout the app and encodes 
	the location of it's definition.

	Most entities can be uniquely accessed by name and type.
	Methods and subroutines have variants with similar 
	names, so a guid is also required to reference procedures.

	Some entities created by the translator are independent
	and not contained by any domain.  Independent entities
	are bound to direct references (see good_ref).
	
	To support adhoc type configuration in phase 4, most
	entities can be virtually copied using v_copy() via
	copy().
	
	Copied entities are complete replicas of the original,
	including domain and sub-domain content, and original 
	location. Reference pointers are copied by value (they
	point to the original entity). The name can be changed
	(or not).  
	
	Copied entities are added to a given spe map which is
	used to translate copied references during review.
	
	See page_reviewer and good_domain for more info.
	

	These entities can be copied virtually:

		good_type (1)
		good_enum_type 
		good_nom_type (1)
		good_subroutine (1)
		good_method (1)
		good_abstract_method (1)
		good_coroutine
		good_object 
		good_literal 
		good_analog  
		good_enumerator 
		good_proxy
		good_operation

		(1)  copy with new name also


	Entities that incorporate references or instructions
	implement v_review(). 

*/
{
	public: // api

		void begin ( void ) // anonymous
		{
			this->descriptive::begin();
			this->name.begin();  // is_pending()
			this->guid.begin();  // is_null()
		}

		void begin ( good_name _cr name, source_guid _cr guid )  // standard
		{
			this->descriptive::begin();
			this->name.begin( name );
			this->guid = guid;
		}

		void begin ( good_entity _cr other )
		{
			this->descriptive::begin();
			this->name.begin( other.name );
			this->guid.begin( other.guid );
		}

		bool has_name () const
		{
			return !this->name.is_pending();
		}

		good_name _cr use_name () const
		{
			return this->name;
		}

		bool has_guid () const
		{
			return !this->guid.is_void();
		}

		source_guid _cr use_guid () const
		{
			return this->guid;
		}
	
		void copy ( spe_map _vr map ) const // same name
		{
			return this->v_copy ( map );
		}

		void copy ( good_name _cr name, spe_map _vr map ) const // new name
		{
			return this->v_copy ( name, map );
		}

		bool review ( page_reviewer _vr reviewer, opcode_t opcode )
		{
			return this->v_review( reviewer, opcode );	
		}

	
	public:  // special

		virtual ~good_entity ()
		{
			this->end();
		}

		good_entity ()
		{
			this->prepare();
		}

		good_entity ( typical_t )
		{
			this->prepare();
			this->begin();
		}

		good_entity ( good_name _cr name, source_guid _cr guid )
		{
			this->prepare();
			this->begin( name, guid );
		}

		good_entity ( good_entity _cr other )
		{
			this->prepare();
			this->begin( other );
		}

		good_entity _vr operator= ( good_entity _cr other ) 
		{
			return _copy_reconstruct( *this, other );
		}


	protected:	// new virtual
	
		virtual void v_copy ( spe_map _vr map ) const  // same name
		{
			_quit( true, bug );  // should override if called
		}

		virtual void v_copy ( good_name _cr name, spe_map _vr map ) const  // new name
		{
			_quit( true, bug );  // should override if called
		}
		
		virtual bool v_review ( page_reviewer _vr reviewer, opcode_t opcode )
		{
			return true;  // nothing to do
		}

	
	protected:  // descriptive

		virtual	string1 v_get_descriptor () const override
		{
			return "entity";
		}

		virtual string1 v_get_description () const override;


	private:

		good_name		name;
		source_guid		guid;

		void prepare ( void )
		{
		}

		void end ( void )
		{
		}

};



inline bool operator< ( sp<good_entity> _cr spe1, sp<good_entity> _cr spe2 ) 
{
	return ( spe1.deref().use_name() < spe2.deref().use_name() );
}
