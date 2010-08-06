#!/usr/bin/env python
import roslib; roslib.load_manifest('PyImageGen')
import rospy
from sensor_msgs.msg import Image
import struct
import cv

def publish(msg):
    pub = rospy.Publisher('/images', Image)
    while not rospy.is_shutdown():
        pub.publish(msg)
        rospy.sleep(3)

def cv_to_ros(img):
    """Convert an OpenCV IplFrame (?) to a ROS Image message. The
       cv_bridge package from the vision_opencv stack does this, but I
       can't build that package on a broken ROS install."""
    if img.nChannels == 3:
        encoding = "rgb8"
    else:
        encoding = "mono8"
    print "Image is %d bytes" % len(img.tostring())
    return Image(width=img.width, height=img.height, \
                 step=img.width*img.nChannels, encoding=encoding, \
                 data=img.tostring())

def pubcam():
    pub = rospy.Publisher("/images", Image)
    capture = cv.CaptureFromCAM(-1)
    while not rospy.is_shutdown():
        frame = cv.QueryFrame(capture)
        pub.publish(cv_to_ros(frame))

if __name__ == '__main__':
    try:
        rospy.init_node('ImageGen')
        pixels = struct.pack("4B",1,2,3,4)
        msg = Image(height=2,width=2,encoding="mono8",step=2,data=pixels)
        publish(msg)
    except rospy.ROSInterruptException:
        pass

