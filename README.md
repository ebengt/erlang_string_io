Allow io module to read/write an Erlang string.

string_io makes it possible to open/2 an Erlang string, like
file:open/2 for a file. The pid() returned can be passed to the io
library module, for reading and writing into the string.

Useful when some module expects to read/write to a file, but you
rather not create a temporary file for this. The other module must, of
course, have an interface that handles an io pid(). As luck would have
it, the one place where I needed this feature, epp_dodger:parse/1, has
such an interface.
