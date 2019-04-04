#! /bin/bash

# Run this from network-framework.

check_exit() {
	if [ "$3" -ne "0" ]; then 
		echo "Failed to $2: $1"
		exit 1
	fi
}

###### Successful scenario ######
./up.sh -s InsuranceService

./invoke.sh testSetup
POLICY="$(./invoke.sh -q requestBid 39 77 10 10 1000)" #Washington, DC. $1000 policy size for moisture level 10.
check_exit "retrieve policy" "$POLICY" $?


BANK="$(./invoke.sh -q getBank)"
check_exit "retrieve Bank"  "$BANK" $?

MONEY=$(./invoke.sh -q withdrawMoney __receiver $BANK 200) # $200 is more than needed.

CHANGE="$(./invoke.sh -q buyPolicy -g PolicyOrGUID $POLICY -g MoneyOrGUID $MONEY)"

WEATHER_RECORD=$(./instantiateOther.sh -q WeatherRecord 39 77 8 10) # 10 seconds after the policy was bought

TIME_SERVICE=$(./invoke.sh -q getTimeService)

./invoke.sh -q setTime __receiver $TIME_SERVICE  864000000 # 10 days in milliseconds

SIGNATURE=$(./instantiateOther.sh -q Signature -g WeatherRecordOrGUID $WEATHER_RECORD)

PAYOUT=$(./invoke.sh -q claim -g PolicyOrGUID $POLICY -g WeatherRecordOrGUID $WEATHER_RECORD -g SignatureOrGUID $SIGNATURE)

PAYOUT_AMT=$(./invoke.sh -q getAmount __receiver $PAYOUT)

echo "Payout amount: $PAYOUT_AMT"
./down.sh