
# -*- coding: utf-8 -*-
from yacc import yacc, lisp_str
import cmd

class MiniLisp(cmd.Cmd):     # See https://docs.python.org/2/library/cmd.html
    """
    MiniLisp evaluates lisp expression,
    more information on http://www.juanjoconti.com.ar
    """

    def __init__(self):
        cmd.Cmd.__init__(self)
        self.prompt = "ml> "
        self.intro  = "Welcome to MiniLisp"

    def do_exit(self, args):
        """Exits from the console"""
        return -1

    def do_EOF(self, args):
        """Exit on system end of file character"""
        print "Good bye!"
        return self.do_exit(args)

    def do_help(self, args):
        print self.__doc__

    def emptyline(self):    
        """Do nothing on empty input line"""
        pass


    def default(self, line):       
        """Called on an input line when the command prefix is not recognized.
           In that case we execute the line as Python code.
        """
        result = yacc.parse(line)

        s = lisp_str(result)
        if s != 'nil':
            print s



if __name__ == '__main__':
        ml = MiniLisp()
        ml.cmdloop()     # See https://docs.python.org/2/library/cmd.html
