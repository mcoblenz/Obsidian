// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    mapping(uint => address) owners;
    uint y;
    
    // This function should NOT be flagged as evidence of state, since the 
    // require argument is dependent on the parameter x.
    // However, x does not show up when getting the dependencies of addr, for some reason.
    function getOwner(uint x) public view returns (address) {
        address addr = owners[x];
        require(addr != address(0));
        return owners[x];
    }
}
