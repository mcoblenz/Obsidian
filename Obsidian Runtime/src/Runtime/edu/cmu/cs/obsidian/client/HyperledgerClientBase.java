package edu.cmu.cs.obsidian.client;

import org.apache.log4j.Logger;
import org.hyperledger.fabric.sdk.HFClient;
import org.hyperledger.fabric_ca.sdk.HFCAClient;
import org.hyperledger.fabric.sdk.security.CryptoSuite;
import org.hyperledger.fabric.sdk.exception.*;
import org.hyperledger.fabric.sdk.Enrollment;
import org.hyperledger.fabric.sdk.Channel;
import org.hyperledger.fabric.sdk.EventHub;
import org.hyperledger.fabric.sdk.Orderer;
import org.hyperledger.fabric.sdk.Peer;
import org.hyperledger.fabric.sdk.User;



import java.net.MalformedURLException;
import java.lang.reflect.InvocationTargetException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Properties;
import java.nio.file.Files;
import java.nio.file.Paths;




// Credit to lkolisko for example code.

public class HyperledgerClientBase {
    private static final Logger log = Logger.getLogger(HyperledgerClientBase.class);

    protected HyperledgerClientConnectionManager connectionManager;

    public void delegatedMain(String[] args) {
        // create fabric-ca client
        HFCAClient caClient = getHfCaClient("http://localhost:7054", null);

        // enroll or load admin
        HyperledgerUser admin = getAdmin(caClient);
        if (admin == null) {
            System.err.println("Exiting client due to failure.");
            System.exit(1);
        }

        log.info(admin);
/*
        // register and enroll new user
        AppUser appUser = getUser(caClient, admin, "hfuser");
        log.info(appUser);

        */
        // get HFC client instance
        HFClient client = getHfClient();
        // set user context
        try {
            client.setUserContext(admin);
        }
        catch (InvalidArgumentException e) {
            System.err.println("Error setting user context: " + e);
            System.exit(1);
        }

        // get HFC channel using the client
        Channel channel = getChannel(client);
        log.info("Channel: " + channel.getName());


        connectionManager = new HyperledgerClientConnectionManager(null); // TODO
    }


    /**
     * Create new HLF client
     *
     * @return new HLF client instance. Never null.
     * @throws CryptoException
     * @throws InvalidArgumentException
     */
    static HFClient getHfClient() {
        try {
            // initialize default cryptosuite
            CryptoSuite cryptoSuite = CryptoSuite.Factory.getCryptoSuite();
            // setup the client
            HFClient client = HFClient.createNewInstance();
            client.setCryptoSuite(cryptoSuite);
            return client;
        }
        catch (Exception e) {
            System.err.println("Failed to get HF client: " + e);
            return null;
        }
    }

    /**
     * Get new fabic-ca client
     *
     * @param caUrl              The fabric-ca-server endpoint url
     * @param caClientProperties The fabri-ca client properties. Can be null.
     * @return new client instance. never null.
     * @throws Exception
     */
    static HFCAClient getHfCaClient(String caUrl, Properties caClientProperties) {
        try {
            CryptoSuite cryptoSuite = CryptoSuite.Factory.getCryptoSuite();
            HFCAClient caClient = HFCAClient.createNewInstance(caUrl, caClientProperties);
            caClient.setCryptoSuite(cryptoSuite);
            return caClient;
        }
        catch (IllegalAccessException |
                MalformedURLException |
                InstantiationException |
                ClassNotFoundException |
                CryptoException |
                InvalidArgumentException |
                NoSuchMethodException |
                InvocationTargetException e) {
            System.err.println("Obsidian bug: exception establishing CA client. " + e);
        }
        return null;
    }

    /**
     * Initialize and get HF channel
     *
     * @param client The HFC client
     * @return Initialized channel
     * @throws InvalidArgumentException
     * @throws TransactionException
     */
    static Channel getChannel(HFClient client) {
        try {
            // initialize channel
            // peer name and endpoint in fabcar network
            Peer peer = client.newPeer("client-peer", "grpc://localhost:7051");
            // eventhub name and endpoint in fabcar network
            EventHub eventHub = client.newEventHub("eventhub01", "grpc://localhost:7053");
            // orderer name and endpoint in fabcar network
            Orderer orderer = client.newOrderer("orderer.example.com", "grpc://localhost:7050");
            // channel name in fabcar network
            Channel channel = client.newChannel("ch1");
            channel.addPeer(peer);
            channel.addEventHub(eventHub);
            channel.addOrderer(orderer);
            channel.initialize();
            return channel;
        }
        catch (InvalidArgumentException | TransactionException e) {
            System.err.println("Unable to get the channel: " + e);
            return null;
        }
    }

    /**
     * Enroll admin into fabric-ca using {@code admin/adminpw} credentials.
     * If AppUser object already exist serialized on fs it will be loaded and
     * new enrollment will not be executed.
     *
     * @param caClient The fabric-ca client
     * @return AppUser instance with userid, affiliation, mspId and enrollment set
     * @throws Exception
     */
    static HyperledgerUser getAdmin(HFCAClient caClient) {
        try {
            HyperledgerUser admin = tryDeserialize("admin");
            if (admin == null) {
                Enrollment adminEnrollment = caClient.enroll("admin", "adminpw");
                if (adminEnrollment == null) {
                    System.err.println("Error enrolling admin.");
                }
                admin = new HyperledgerUser("admin", "org1", "DEFAULT", adminEnrollment);

                User.userContextCheck(admin);
                serialize(admin);
            }
            return admin;
        }
        catch (Exception e) {
            System.err.println("Error getting user: " + e);
            return null;
        }
    }


    // user serialization and deserialization utility functions
    // files are stored in the base directory

    /**
     * Serialize HyperledgerUser object to file
     *
     * @param appUser The object to be serialized
     * @throws IOException
     */
    static void serialize(HyperledgerUser appUser) throws IOException {
        try (ObjectOutputStream oos = new ObjectOutputStream(Files.newOutputStream(
                Paths.get(appUser.getName() + ".jso")))) {
            oos.writeObject(appUser);
        }
    }

    /**
     * Deserialize HyperledgerUser object from file
     *
     * @param name The name of the user. Used to build file name ${name}.jso
     * @return
     * @throws Exception
     */
    static HyperledgerUser tryDeserialize(String name) throws Exception {
        if (Files.exists(Paths.get(name + ".jso"))) {
            return deserialize(name);
        }
        return null;
    }

    static HyperledgerUser deserialize(String name) throws Exception {
        try (ObjectInputStream decoder = new ObjectInputStream(
                Files.newInputStream(Paths.get(name + ".jso")))) {
            return (HyperledgerUser) decoder.readObject();
        }
    }
}