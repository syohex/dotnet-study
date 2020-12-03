using System;
using System.Threading;

namespace ThreadThis
{
    class Foo
    {
        private string _name;
        Thread _thread;
        public Foo(string name)
        {
            _name = name;
            _thread = new Thread(ThreadFunc);
            _thread.Start(299);
        }
        public ~Foo()
        {
            Console.WriteLine("Destructor");
        }

        private void ThreadFunc(object age)
        {
            Console.WriteLine("my name is {0} and age is {1}", _name, age);
        }


    }

    public class Program
    {
        static void Main(string[] args)
        {
            var f = new Foo("tom");
            f.Finalize();
            Console.WriteLine("Hello World!");
        }
    }
}
