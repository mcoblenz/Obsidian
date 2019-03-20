/**
  * Created by Miles Baker on 6/14/2018.
  * Inherits from the Hyperledger ChaincodeBase class, and holds any static Java code that
  * we might want to be common to all chaincode classes.
  *//**
  * Created by Miles Baker on 6/14/2018.
  * Inherits from the Hyperledger ChaincodeBase class, and holds any static Java code that
  * we might want to be common to all chaincode classes.
  */
package edu.cmu.cs.obsidian.chaincode

import java.nio.charset.StandardCharsets.UTF_8
import com.google.protobuf.InvalidProtocolBufferException
import org.json._
import java.io._
import java.net.ServerSocket
import java.net.Socket
import java.nio.file.StandardOpenOption
import java.util
import java.util.Base64
import java.lang.reflect.Constructor
import java.lang.reflect.InvocationTargetException
import org.hyperledger.fabric.shim.ChaincodeBase
import org.hyperledger.fabric.shim.ChaincodeStub
import org.hyperledger.fabric.shim.ledger._
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
import edu.cmu.cs.obsidian.chaincode.SerializationState

abstract class HyperledgerChaincodeBase() extends Nothing with Nothing {
    serializationState = new Nothing
    private[chaincode] var serializationState = null

    // For cross-contract instantiation.
    def this(s: Nothing) {
        this()
        serializationState = s
    }

    def flush(): Unit = {
        // No need to do anything because main contracts aren't lazily loaded.
    }

    @Override def init(stub: Nothing): Nothing = {
        serializationState.setStub(stub)
        val function = stub.getFunction
        if (function.equals("init")) try {
            val args = stub.getParameters.stream.toArray(`new`)
            val byte_args = new Array[Array[Byte]](args.length)
            var i = 0
            while ( {
                i < args.length
            }) {
                byte_args(i) = args(i).getBytes
                {
                    i += 1; i - 1
                }
            }
            val result = init(serializationState, byte_args)
            System.out.println("contract init completed")
            __saveModifiedData(stub)
            System.out.println("after __saveModifiedData")
            serializationState.flushEntries
            System.out.println("after flushEntries")
            newSuccessResponse(result)
        } catch {
            case e: Nothing =>
                newErrorResponse(e)
        }
        else newErrorResponse("Unknown initialization function " + function)
    }

    @Override def invoke(stub: Nothing): Nothing = {
        serializationState.setStub(stub)
        val function = stub.getFunction
        var args = stub.getParameters.stream.toArray(`new`)
        // If this invocation is really to a different contract, figure that out.
        var invocationReceiver = this
        if (args.length > 0) {
            val firstArg = args(0)
            if (firstArg.equals("__receiver")) { // Expect the second arg to be the GUID of the reciever.
                if (args.length > 1) {
                    val receiverGUID = args(1)
                    var receiverContract = serializationState.getEntry(receiverGUID)
                    // If it's not in our map, maybe we just haven't loaded it yet.
                    if (receiverContract == null) {
                        val objectClass = serializationState.getReturnedObjectClass(stub, receiverGUID)
                        if (objectClass == null) return newErrorResponse("Can't find object with ID: " + receiverGUID)
                        else try {
                            val constructor = objectClass.getConstructor(classOf[Nothing])
                            receiverContract = constructor.newInstance(receiverGUID).asInstanceOf[Nothing]
                            serializationState.putEntry(receiverGUID, receiverContract)
                        } catch {
                            case e@(_: Nothing | _: Nothing | _: Nothing | _: Nothing) =>
                                return newErrorResponse("Failed to instantiate archived object: " + e)
                        }
                    }
                    if (receiverContract == null) return newErrorResponse("Cannot invoke transaction on unknown object " + receiverGUID)
                    else if (receiverContract.isInstanceOf[Nothing]) invocationReceiver = receiverContract.asInstanceOf[Nothing]
                    else return newErrorResponse("Cannot invoke transaction on non-contract " + receiverContract)
                    if (args.length > 2) args = Arrays.copyOfRange(args, 2, args.length - 1)
                    else args = new Array[Nothing](0)
                }
                else return newErrorResponse("Invoking on a non-main contract requires specifying a receiver.")
            }
            else if (firstArg.equals("__instantiateOther")) { // Expect the second arg to be the contract name of the reciever.
                if (args.length > 1) {
                    val otherContractName = args(1)
                    val otherClassName = "org.hyperledger.fabric.example." + otherContractName
                    val argBytes = stub.getParameters.stream.toArray(`new`)
                    var restArgs = null
                    if (argBytes.length > 2) restArgs = Arrays.copyOfRange(argBytes, 2, argBytes.length - 1)
                    else restArgs = new Array[Array[Byte]](0)
                    System.out.println("attempting to instantiate other class " + otherClassName)
                    val newContractGUID = instantiateOtherContract(otherClassName, restArgs)
                    if (newContractGUID != null) return newSuccessResponse(newContractGUID.getBytes(java.nio.charset.StandardCharsets.UTF_8))
                    else return newErrorResponse("Failed to instantiate contract " + otherContractName)
                }
                else return newErrorResponse("Instantiating another contract requires specifying a contract class name.")
            }
        }
        val byte_args = new Array[Array[Byte]](args.length)
        var i = 0
        while ( {
            i < args.length
        }) {
            byte_args(i) = args(i).getBytes
            {
                i += 1; i - 1
            }
        }
        try {
            /* Try to restore ourselves (the root object) from the blockchain
                        * before we invoke a transaction. (This applies if we stopped the
                        * chaincode and restarted it -- we have to restore the state of
                        * the root object.) */ __restoreObject(serializationState)
            val result = invocationReceiver.run(serializationState, function, byte_args)
            __saveModifiedData(stub)
            newSuccessResponse(result)
        } catch {
            case e: Nothing =>
                /* This will be returned when calling an invalid transaction
                             * from the command line -- referencing an invalid transaction
                             * in the client will give a compile-time error. */ newErrorResponse("No such transaction: " + function)
            case e: Nothing =>
                newErrorResponse(e)
        }
    }

    def delegatedMain(args: Array[Nothing]): Unit = /* spin up server */ try {
        System.out.println("Args: " + args)
        start(args)
    } catch {
        case e: Nothing =>
            System.out.println("Error: Exception raised when running server: " + e)
            System.exit(1)
    }

    // Returns the GUID of the new instance if initialization was successful, and null otherwise.
    def instantiateOtherContract(contractClassName: Nothing, args: Array[Array[Byte]]): Nothing = {
        if (!contractClassName.startsWith("org.hyperledger.fabric.example")) { // We don't permit looking up arbitrary Java classes for security reasons!
            return null
        }
        try {
            System.out.println("Looking for a class for name: " + contractClassName)
            val foundClass = Class.forName(contractClassName)
            System.out.println("Found class to instantiate: " + foundClass)
            val constructor = foundClass.getConstructor(classOf[Nothing])
            val contract = constructor.newInstance(serializationState)
            System.out.println("Instantiated contract: " + contract)
            try {
                contract.init(serializationState, args)
                System.out.println("init complete")
            } catch {
                case e: Nothing =>
                    System.err.println("Unable to initialize contract: " + e)
                    return null
            }
            contract.__getGUID
        } catch {
            case e@(_: Nothing | _: Nothing | _: Nothing | _: Nothing | _: Nothing) =>
                System.err.println("Unable to instantiate contract: " + e)
                null
        }
    }

    /* Figure out what was modified and write it out to the blockchain.
         * Only called for main transactions. */ def __saveModifiedData(stub: Nothing): Unit = {
        val dirtyFields = __resetModified(new Nothing)
        System.out.println("after __resetModified")
        import scala.collection.JavaConversions._
        for (field <- dirtyFields) {
            /* Find key and bytes to archive for each dirty field. */ val archiveKey = field.__getGUID
            val archiveValue = new Nothing(field.__archiveBytes)
            System.out.println("Saving modified data: (" + field + " @ <" + archiveKey + "> => " + archiveValue + ")")
            stub.putStringState(archiveKey, archiveValue)
        }
        __unload()
    }

    // Must be overridden in generated class.
    def __resetModified(checked: Nothing): Nothing

    def __getGUID: Nothing

    @throws[InvalidProtocolBufferException]
    def init(st: Nothing, args: Array[Array[Byte]]): Array[Byte]

    @throws[InvalidProtocolBufferException]
    @throws[ReentrancyException]
    @throws[BadTransactionException]
    @throws[NoSuchTransactionException]
    def run(st: Nothing, transactionName: Nothing, args: Array[Array[Byte]]): Array[Byte]

    @throws[InvalidProtocolBufferException]
    def __initFromArchiveBytes(archiveBytes: Array[Byte], __st: Nothing): HyperledgerChaincodeBase

    def __archiveBytes: Array[Byte]

    @throws[InvalidProtocolBufferException]
    def __restoreObject(st: Nothing): Unit

    protected def __unload(): Unit
}