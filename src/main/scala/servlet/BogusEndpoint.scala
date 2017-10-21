package servlet

class BogusEndpoint {

}


// http://svprre02:8080/fuseki/tdb/sparql?query=SELECT++*%0AWHERE%0A++%7B+%3Fs++%3Fp++%22dom%22+%7D%0A

// response structure (JSON-ld):

/*

{
  "head": {
    "vars": [ "s" , "p" ]
  } ,
  "results": {
    "bindings": [
      {
        "s": { "type": "uri" , "value": "http://www.ivdnt.org/diamant#gek.dom" } ,
        "p": { "type": "uri" , "value": "http://www.ivdnt.org/diamant#target" }
      } ,
 */