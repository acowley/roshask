This project shows the simple interface to the service client. Main.hs calls the AddTwoInts service from the ROS tutorials (http://wiki.ros.org/ROS/Tutorials/WritingServiceClient). You can find service definition and the original server code in the rospy_tutorials ROS package.

Before you begin, install roshask. Directions are on the GitHub wiki: https://github.com/acowley/roshask/wiki

To run src/Main.hs:

1. cd <path to roshask>/Examples/AddTwoIntsClient
2. run "roshask dep" (this generates the Haskell Request and Response types from rospy_tutorials/srv/AddTwoInts.srv)
3. In another terminal run "roscore": This starts the ROS master.
4.a In another terminal cd <path to roshask>/Examples/AddTwoIntsClient/src
4.b run "python add_two_ints.py": This starts the add_two_ints server.
3. In the original terminal run "cabal run": This compiles and runs "Main.hs". You should see "Right (AddTwoIntsResponse {sum = 15})" printed out. In the termial where add_two_ints_server.py is running you should see "Returning [10 + 5 = 15]". You can also do "runhaskell Main.hs" in Examples/AddTwoIntsClient/src.
