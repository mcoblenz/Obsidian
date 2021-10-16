
object "IfThenElse" {
    code {
        
        mstore(64, 128)
        
        if callvalue() {revert(0, 0)}
        
        
        
        codecopy(0, dataoffset("IfThenElse_deployed"), datasize("IfThenElse_deployed"))
        return(0, datasize("IfThenElse_deployed"))
    }
    object "IfThenElse_deployed" {
        code {
            
            mstore(64, 128)
            

            
            if iszero(lt(calldatasize(), 4)) {
                    
                    let selector := shr(224, calldataload(0))
                    switch selector
case 0x674cd1ac {if callvalue() {revert(0, 0)}
abi_decode_tuple(4, calldatasize())
ifthenelse()
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
                

function ifthenelse() {
        let x
        let _tmp_1
        let _tmp_2
        _tmp_2 := true
        switch _tmp_2
case true {let _tmp_3
_tmp_3 := 1
x := _tmp_3}
case false {let _tmp_4
_tmp_4 := 0
x := _tmp_4}

        leave
}

                

function main() -> _ret_1 {
        let _tmp_5
        _tmp_5 := 9
        let x := _tmp_5
        let _tmp_6
        let _tmp_7
        _tmp_7 := false
        switch _tmp_7
case true {let _tmp_8
_tmp_8 := 50
x := _tmp_8}
case false {let _tmp_9
_tmp_9 := 90
x := _tmp_9}

        let _tmp_10
        _tmp_10 := x
        _ret_1 := _tmp_10
        leave
}

        }
        
    }
    
}