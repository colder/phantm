package phpanalysis;

class JavaListIteratorWrapper[A](iter:java.util.Iterator[A])
{
    def foreach(f: A => Unit): Unit = {
        while(iter.hasNext){
          f(iter.next)
        }
    }
}

