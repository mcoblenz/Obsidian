
object "AssignLocalAdd" {
    code {
        
        mstore(64, 128)
        
        if callvalue() {revert(0, 0)}
        
        
        
        codecopy(0, dataoffset("AssignLocalAdd_deployed"), datasize("AssignLocalAdd_deployed"))
        return(0, datasize("AssignLocalAdd_deployed"))
    }
    object "AssignLocalAdd_deployed" {
        code {
            
            mstore(64, 128)
            

            
            if iszero(lt(calldatasize(), 4)) {
                    
                    let selector := shr(224, calldataload(0))
                    switch selector
case 0xddbbec23 {if callvalue() {revert(0, 0)}
abi_decode_tuple(4, calldatasize())
assignlocaladd()
let memPos := allocate_unbounded()
let memEnd := abi_encode_tuple_to_fromStack0(memPos)
return(memPos, sub(memEnd, memPos))}
case 0xdffeadd0 {if callvalue() {revert(0, 0)}
abi_decode_tuple(4, calldatasize())
let _dd_ret_1 := main()
let memPos := allocate_unbounded()
let memEnd := abi_encode_tuple_to_fromStack1(memPos, _dd_ret_1)
return(memPos, sub(memEnd, memPos))}

            }
            if iszero(calldatasize()) {  }
            revert(0, 0)
            
            
            function abi_decode_tuple(headStart, dataEnd)   {
                if slt(sub(dataEnd, headStart), 0) { revert(0, 0) }
            }
            

function abi_encode_tuple_to_fromStack0(headStart ) -> tail {
        tail := add(headStart, 0)
}



function abi_encode_tuple_to_fromStack1(headStart , value0) -> tail {
        tail := add(headStart, 32)
        abi_encode_t_uint256_to_t_uint256_fromStack(value0, add(headStart, 0))
}


            function allocate_memory(size) -> memPtr {
                memPtr := allocate_unbounded()
                finalize_allocation(memPtr, size)
            }

            function allocate_unbounded() -> memPtr {
                memPtr := mload(64)
            }

            function finalize_allocation(memPtr, size) {
                let newFreePtr := add(memPtr, round_up_to_mul_of_32(size))
                // protect against overflow
                if or(gt(newFreePtr, 0xffffffffffffffff), lt(newFreePtr, memPtr)) {  panic_error_0x41() }
                mstore(64, newFreePtr)
            }

            function panic_error_0x41() {
                mstore(0, 35408467139433450592217433187231851964531694900788300625387963629091585785856)
                mstore(4, 0x41)
                revert(0, 0x24)
            }

            function round_up_to_mul_of_32(value) -> result {
                result := and(add(value, 31), not(31))
            }

            function abi_encode_t_uint256_to_t_uint256_fromStack(value, pos) {
                mstore(pos, cleanup_t_uint256(value))
            }

            
            function cleanup_t_uint256(value) -> cleaned {
                cleaned := value
            }
                

function assignlocaladd() {
        let x
        let y
        let _tmp_1
        let _tmp_2
        let _tmp_3
        _tmp_2 := 5
        _tmp_3 := 12
        _tmp_1 := add(_tmp_2, _tmp_3)
        x := _tmp_1
        leave
}

                

function main() -> _ret_1 {
        let _tmp_4
        let _tmp_5
        let _tmp_6
        _tmp_5 := 5
        _tmp_6 := 12
        _tmp_4 := add(_tmp_5, _tmp_6)
        let x := _tmp_4
        let _tmp_7
        _tmp_7 := x
        _ret_1 := _tmp_7
        leave
}

        }
        
    }
    
}