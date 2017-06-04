package io.lglenn;

trait Stack {

    def push(el: Int): Stack = Stack(el,this);
    def pop: (Int, Stack);
    def peek: Int;
    def height: Int;

}

class IntStack(head: Int, tail: Stack) extends Stack {

    def pop: (Int, Stack) = (head,tail);
    def peek: Int = head;
    def height: Int = 1 + tail.height;
    override def toString: String = s"${tail} | ${head}"

}

object EmptyStack extends Stack {
    def pop: (Int, Stack) = throw new Exception;
    def peek: Int = throw new Exception;
    def height: Int = 0;
    override def toString: String = s"#"
}

object Stack {

    def apply(head: Int, tail: Stack) = new IntStack(head, tail);

}
