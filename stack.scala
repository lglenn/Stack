package io.lglenn;

sealed trait Stack[+A] {
    def pop: (A, Stack[A]);
    def peek: A;
    def push[B>:A](head: B): Stack[B] = Stack[B](head,this);
    def +: [B>:A](head: B): Stack[B] = push(head)
    def height: Int;
    def isEmpty;
}

class StackFrame[+A](val head: A, val tail: Stack[A]) extends Stack[A] {
    def pop: (A, Stack[A]) = (head,tail);
    def peek: A = head;
    def height: Int = 1 + tail.height;
    def isEmpty = false;
    override def toString: String = s"${head} | ... | #"
}

object Base extends Stack[Nothing] {
    def pop: (Nothing, Stack[Nothing]) = throw new Exception;
    def peek: Nothing = throw new Exception;
    def height: Int = 0;
    def isEmpty = true;
    override def toString: String = s"#"
}

object Stack {
    def apply[A](head: A, tail: Stack[A]) = new StackFrame[A](head, tail);
}

object +: {
    def unapply[A](stack: Stack[A]): Option[(A, Stack[A])] =
        if (stack.isEmpty) None else Some(stack.pop);
}
