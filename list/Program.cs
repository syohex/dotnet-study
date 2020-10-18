namespace list
{
    class Program
    {
        static void Main(string[] args)
        {
            var lst = new List(1);
            lst.InsertTail(1000);
            lst.Print();
            lst.InsertHead(500);
            lst.InsertHead(600);
            lst.InsertHead(700);
            lst.Print();

            lst.Insert(2, 42);
            lst.Print();
        }
    }
}
