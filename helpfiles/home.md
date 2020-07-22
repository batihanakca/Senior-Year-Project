### **Entering should be strictly as following**

**Week Number:** This should be the cumulative week number, you can find it from View>>Date View module.

**Year:** Year.

**Overall Service Level:** This is the overall system service level requirement.

**Specification:** This is the specification for the SKU's with free lot size. For example, 0.5 specification means lot size = MOQ/2 lower specification might generate unsolvable model. The recommended value is 0.5.



### Before running the module, the following data should be present in the database

**SKU related information:** Code, type, holding cost, MOQ, lot size, individual service level, weekly sigma.

**Forecast:** Forecast for the current week

**Pre-Order:** The order from the previous week should be uploaded to the database before running the model.

**Sales:** The finalized sales of the previous week should be uploaded.

 