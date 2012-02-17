extern "C++" 
{ 
  namespace std 
  { 
    class exception 
    { 
    public:exception () throw () 
      { 
      } virtual ~ exception () throw (); 
      virtual const char *what () const throw (); 
    }; 
  } 
  namespace __cxxabiv1 
  { 
  } 
  namespace std 
  { 
    class bad_cast:public exception 
    { 
    public:bad_cast () throw () 
      { 
      } virtual ~ bad_cast () throw (); 
    }; 
  } 
} 
namespace __cxxabiv1 
{ 
  extern "C" void __cxa_bad_cast (); 
} 
extern "C" void 
__cxxabiv1::__cxa_bad_cast () 
{ 
  throw std::bad_cast (); 
}
