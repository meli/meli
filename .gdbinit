set language rust
source ~/.gdbinit
break rust_panic
break core::option::expect_failed::h4927e1fef06c4878
break core::panicking::panic
break libcore/panicking.rs:58
break libcore/result.rs:945
set auto-load python-scripts
break melib/src/mailbox/thread.rs:1010
set print thread-events off

#python
#import os
#import sys
#
#sys.path.insert(0, os.getcwd() + '/scripts/gdb_meli/')
#import gdb
#import gdb_meli
#
#print(gdb_meli.__file__)
#
#help(gdb_meli)
##from gdb_meli import build_pretty_printer
##print(gdb.objfiles()[0].filename)
##gdb_meli.register_pretty_printer(gdb)
##gdb.printing.register_pretty_printer(
##    gdb.current_objfile(),
##    gdb_meli.build_pretty_printer())
#end
python

import sys, os

sys.path.insert(0, os.getcwd() + '/scripts/gdb_meli/')


import gdb_meli, gdb
#gdb_meli.register_meli_printers(gdb)
#gdb.printing.register_pretty_printer(
#	 gdb.current_objfile(),
#	 gdb_meli.build_meli_printer())
end
