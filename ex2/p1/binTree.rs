use std::io; 
use std::cmp::PartialOrd;
use std::cmp::PartialEq; 


#[derive(PartialEq,PartialOrd, Debug)] 
enum BinTree<'a, T> { 
    Node(T,&'a BinTree<'a, T>, &'a BinTree<'a, T>),
    Leaf(T),
}

fn checkBST(&BinTree<'a, T> tree) -> bool { 
   match tree { 
        Node(l,r) => .. ,
        Leaf(n) => .. ,
}

fn main() {
    let a = BinTree::Leaf(5); 
    println!("hola {:?}",a); 
}
