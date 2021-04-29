class Node:
    def __init__(self, value):
        self.value = value
        self.next = None 

class Linkedlist:
    def __init__(self):
        self.head = None
    
    def insert(self, node):
        curr = self.head
        if curr == None:
            self.head = node
            return 
        while curr.next is not None:
            curr = curr.next 
        curr.next = node 
    
    def traverse(self):
        curr = self.head
        while curr is not None:
            print(curr.value)
            curr = curr.next

    def reverse(self):
        if self.head == None:
            return 
        next = self.head.next
        if next == None:
            return 
        newHead = self.reverse_helper(self.head, next)
        self.head.next = None
        self.head = newHead
    
    def reverse_helper(self, prev, curr):
        next = curr.next 
        curr.next = prev
        if next == None:
            return curr
        return self.reverse_helper(curr, next)
        


a = Linkedlist()
a.insert(Node(1))
a.insert(Node(2))
a.insert(Node(3))
a.insert(Node(4))
a.reverse()
a.traverse()
