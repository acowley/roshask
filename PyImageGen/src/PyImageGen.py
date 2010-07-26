#!/usr/bin/env python
import roslib; roslib.load_manifest('PyImageGen')
import rospy
from sensor_msgs.msg import Image
import struct

def publish(msg):
    pub = rospy.Publisher('/images', Image)
    while not rospy.is_shutdown():
        pub.publish(msg)
        rospy.sleep(3)

if __name__ == '__main__':
    try:
        rospy.init_node('ImageGen')
        pixels = struct.pack("4B",1,2,3,4)
        msg = Image(height=2,width=2,encoding="mono8",step=2,data=pixels)
        publish(msg)
    except rospy.ROSInterruptException:
        pass

