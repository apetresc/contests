import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

class BinarySearchTree<T extends Comparable<T>> {
    private Node<T> root = null;
    private int size = 0;

    /**
     * Insert a value into the tree.
     * 
     * @param value the value to insert
     */
    void insert(T value) {
        if (root == null) {
            root = new Node<T>(value);
            size = 1;
        } else {
            insert(root, value);
        }
    }

    /**
     * Insert a value into the tree rooted at node.
     * 
     * @param node the root of the tree to insert into
     * @param value the value to insert
     */
    void insert(Node<T> node, T value) {
        if (value.compareTo(node.getData()) <= 0) {
            if (node.getLeft() == null) {
                node.left = new Node<T>(value);
                size++;
            } else {
                insert(node.getLeft(), value);
            }
        } else {
            if (node.getRight() == null) {
                node.right = new Node<T>(value);
                size++;
            } else {
                insert(node.getRight(), value);
            }
        }
    }

    interface Visitor<T> {
        void visit(Node<T> node);
    }

    /**
     * Visit each node in the tree in depth-first order. Calls the passed visitor
     * 
     * @param visitor the visitor to call for each node
     */
    void depthFirstTraversal(Visitor<T> visitor) {
        depthFirstTraversal(root, visitor);
    }

    void depthFirstTraversal(Node<T> node, Visitor<T> visitor) {
        if (node == null) {
            return;
        }

        depthFirstTraversal(node.getLeft(), visitor);
        visitor.visit(node);
        depthFirstTraversal(node.getRight(), visitor);
    }

    /**
     * Visit each node in the tree in breadth-first order. Calls the passed visitor
     * 
     * @param visitor the visitor to call for each node
     */
    void breadthFirstTraversal(Visitor<T> visitor) {
        breadthFirstTraversal(new LinkedList<>(Collections.singleton(root)), visitor);
    }

    void breadthFirstTraversal(List<Node<T>> q, Visitor<T> visitor) {
        while (!q.isEmpty()) {
            Node<T> node = q.remove(0);
            visitor.visit(node);

            if (node.getLeft() != null) {
                q.add(node.getLeft());
            }

            if (node.getRight() != null) {
                q.add(node.getRight());
            }
        }
    }

    /**
     * Convert the tree into a list.
     * 
     * @return a list containing the values of the tree in sorted order
     */
    List<T> getAsSortedList() {
        List<T> list = new ArrayList<T>(this.size);
        this.depthFirstTraversal((node) -> list.add(node.getData()));
        return list;
    }

    List<T> getAsLevelOrderList() {
        List<T> list = new ArrayList<T>(this.size);
        this.breadthFirstTraversal((node) -> list.add(node.getData()));
        return list;
    }

    Node<T> getRoot() {
        return root;
    }

    static class Node<T> {
        private final T data;
        private Node<T> left = null;
        private Node<T> right = null;

        Node(T data) {
          this.data = data;
        }

        Node<T> getLeft() {
          return left;
        }

        Node<T> getRight() {
          return right;
        }

        T getData() {
          return data;
        }

    }
}
