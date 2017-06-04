package io.lglenn;

sealed trait Stack[+A] {
    def pop: (A, Stack[A]);
    def peek: A;
    def push[B>:A](head: B): Stack[B] = Stack[B](head,this);
    def height: Int;
}

class StackFrame[+A](val head: A, val tail: Stack[A]) extends Stack[A] {

    def pop: (A, Stack[A]) = (head,tail);
    def peek: A = head;
    def height: Int = 1 + tail.height;
    override def toString: String = s"${tail} | ${head}"

}

object EmptyStack extends Stack[Nothing] {
    def pop: (Nothing, Stack[Nothing]) = throw new Exception;
    def peek: Nothing = throw new Exception;
    def height: Int = 0;
    override def toString: String = s"#"
}

object Stack {

    def apply[A](head: A, tail: Stack[A]) = new StackFrame[A](head, tail);

}
