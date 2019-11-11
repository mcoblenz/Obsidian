pragma solidity >=0.5.11;

/*
 * This system is intended to reflect the following rule on prescriptions:
 *    A prescription cannot be filled more times than the designated number of refills specifies.
 *
 * However, the Drug Enforcement Agency is worried that the code below might allow violations of the rule by
 * malevolent patients, who might modify or subclass the Patient contract.
 * Specifically, it is possible for a caller to call depositPrescription() several times
 * on different Pharmacy objects with the same Prescription object. See getMedicine() below for an example.
 *
 * YOUR TASK: Please use what you have learned today to fix this problem (avoiding runtime checks if possible).
 */

contract DoctorRegistry {
    mapping(address => bool) approvedDoctors; // Only these doctors can sign a prescription.
    address owner;

    constructor () public {
        owner = msg.sender;
    }

    function doctorIsApproved(address doctor) public view returns (bool) {
        return approvedDoctors[doctor];
    }

    function addApprovedDoctor(address doctor) public {
        require(msg.sender == owner, "Only the owner can approve a doctor.");
        approvedDoctors[doctor] = true;
    }
}

// Prescriptions are immutable by design; pharmacies that want to record information about
// prescriptions must do so in a separate data structure.
// Only approved doctors can create new prescriptions.
contract Prescription {
    string public prescriptionText; // specifies the drug, dosage, etc.
    int public refills;
    string public patientName; // Assume that patient names are unique.
    address authorizingDoctor;

    constructor (DoctorRegistry dr, string memory text, int r, string memory patient) public {
        require (dr.doctorIsApproved(msg.sender), "Only approved doctors can create prescriptions.");

        authorizingDoctor = msg.sender;
        prescriptionText = text;
        refills = r;
        patientName = patient;
    }
}


/* A Pharmacy keeps a record of all the prescriptions that patients have submitted as well as how many times each prescription has been filled.
 * Before filling a prescription, the pharmacy must make sure that there is at least one refill available.
 */
contract Pharmacy {
    struct PharmacyPrescriptionRecord {
        int fillsLeft;
        Prescription prescription;
    }

    int nextID;

    constructor() public {
        nextID = 0;
    }

    mapping(int => PharmacyPrescriptionRecord) prescriptionRecords;

	// Must be called before the patient can fill a new prescription.
    // Assumes that the prescription was not already deposited in this Pharmacy.
	function depositPrescription(Prescription prescription) public returns (int) {
        int id = nextID;
        int origFills = prescription.refills();
        addRecord(id, PharmacyPrescriptionRecord(origFills, prescription));
        nextID++;
	}

    function removeRecord(int prescriptionID) private returns (PharmacyPrescriptionRecord memory) {
        PharmacyPrescriptionRecord memory record = prescriptionRecords[prescriptionID];
        delete prescriptionRecords[prescriptionID];
        return record;
    }

    function addRecord(int prescriptionID, PharmacyPrescriptionRecord memory record) private {
        prescriptionRecords[prescriptionID] = record;
    }

	function fillPrescription(int prescriptionID) public {
        PharmacyPrescriptionRecord memory record = removeRecord(prescriptionID);
        if (record.fillsLeft > 0) {
            record.fillsLeft = record.fillsLeft - 1;
            addRecord(prescriptionID, record);
            requestFillFromPharmacist(prescriptionID);
        }
	}

	function requestFillFromPharmacist(int prescriptionID) public {
		// This code, which is not shown, notifies the pharmacist to actually fill the prescription.
	}
}

// Do not change the code below here. Instead, change the code ABOVE to defend against the code below.
contract Patient {
    function getMedicine(Pharmacy pharmacy1, Pharmacy pharmacy2, Prescription prescription) public {
        int prescriptionID1 = pharmacy1.depositPrescription(prescription);
        pharmacy1.fillPrescription(prescriptionID1);

        // Uh oh: the patient should not be able to deposit the prescription
        // a second time, obtaining more refills than authorized!
        int prescriptionID2 = pharmacy2.depositPrescription(prescription);
        pharmacy2.fillPrescription(prescriptionID2);
    }
}

