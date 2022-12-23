pragma solidity >=0.8.0;

// Based on https://medium.com/coinmonks/linked-lists-in-solidity-cfd967af389b

contract LinkedList {

  event AddEntry(bytes32 head,uint number,bytes32 next);

  uint public length = 0;//also used as nonce

  struct Object{
    bytes32 next;
    uint number;
  }

  bytes32 public head;
  mapping (bytes32 => Object) public objects;

  constructor (){}

  function addEntry(uint _number) public returns (bool){
    Object memory object = Object(head,_number);
    bytes32 id = keccak256(abi.encode(object.number,block.timestamp,length));
    objects[id] = object;
    head = id;
    length = length+1;
    return true;
  }

  //needed for external contract access to struct
  function getEntry(bytes32 _id) public view returns (bytes32,uint){
    return (objects[_id].next,objects[_id].number);
  }


    function main() public {
        addEntry(1);
        addEntry(1);
        addEntry(1);
        addEntry(1);
        addEntry(1);
        addEntry(1);
        addEntry(1);
        addEntry(1);
    }

}