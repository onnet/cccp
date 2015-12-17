CouchDB system_config doc:

   {
      "_id": "cccp",
   
      "_rev": "22-e4e3d5976b62807677ea5e2692431f60",
   
      "default": {
   
          "cccp_cb_number": "78127481093",
   
          "cccp_cc_number": "78127481000",
   
          "last_number_redial_code": "*0",
   
          "ensure_valid_caller_id": true,
   
          "default_caller_id_number": "00000000000",
   
          "allowed_callee_regex": "^\\+?\\d{11,}$",
   
          "callback_delay": 3
   
      },
   
      "pvt_account_id": "system_config",
   
      "pvt_account_db": "system_config",
   
      "pvt_created": 63582529472,
   
      "pvt_modified": 63617518172,
   
      "pvt_type": "config",
   
      "pvt_node": "whistle_apps@kz527.onnet.su"
   }


API call initiation:

curl -X PUT -H X-Auth-Token:da3fda2fb178365d206ab28b0a4a4a4c https://kz527.onnet.su:8443/v1/accounts/33ca3929ed585e0e423eb39e4ffe1452/cccps/autodial -d '{"data": { "a_leg_number": "9169944", "outbound_cid": "78122404700", "b_leg_number": "5579", "callback_delay": 20}}'

