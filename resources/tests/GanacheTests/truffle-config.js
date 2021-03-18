// this is a config file to copy into each truffle project so that it runs
// fabric-evm code and works on the development network, provided by Ganache.

module.exports = {
  networks: {
    development: {
      host: "127.0.0.1",
      port: "8545", // standard ethereum port, i think
      network_id: "*",
      type: "fabric-evm"
    }
  }

  // i believe these are the defaults but they may need to change to work for us. specifically the docker setting.
  compilers: {
    solc: {
      // version: "0.5.1",    // Fetch exact version from solc-bin (default: truffle's version)
      // docker: true,        // Use "0.5.1" you've installed locally with docker (default: false)
      // settings: {          // See the solidity docs for advice about optimization and evmVersion
      //  optimizer: {
      //    enabled: false,
      //    runs: 200
      //  },
      //  evmVersion: "byzantium"
      // }
    }
  }
};
