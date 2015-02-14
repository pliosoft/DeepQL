# DeepQL

I expect this to eventually grow into a full-stack compiler for webapps. part of a pie-in-sky-project we're building.

## What is it?

Take a specification, run it through the compiler, and a full-stack web application pops out, 
with a React-style front-end, a REST api server, and all of the schema required to bootstrap your database.

## Example Input

here's what the database schema portion would look like:

   # The student we're dealing with.
   node Student {
       @vendorid:  String # iOS device's reported id
            name:  String
          emails: [String] # May have many emails
           phone:  String
          school:  School
         .secret: # Not returned in public queries, but the server can access it
            {
               hash:  String
               salt:  String
            }
   }


