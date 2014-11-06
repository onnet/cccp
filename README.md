cccp
====
Calling Card Callback Platform

Could be good for road warriors:

- give a call to the customer as if you are calling from your office, i.e. hide behind your companie's CID
  (Unlike to cf_disa, no callflows should be created at customer's accounts);

- relay long distance calls expences to your company instead of your own using callback;


HOW-TO

1. cd /opt/kazoo/applications/
2. git clone https://github.com/onnet/cccp.git
3. cd cccp
4. sh priv/copy_to_outside.sh
5. cd /opt/kazoo
6. make
7. sup whapps_maintenance refresh
8. sup whistle_maintenance hotload cb_cccps
9. sup crossbar_maintenance start_module cb_cccps
10. sup whapps_controller start_app cccp
