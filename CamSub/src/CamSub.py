#!/usr/bin/env python
import roslib; roslib.load_manifest('CamSub')
import rospy
from sensor_msgs.msg import Image
from cv_bridge import CvBridge
import cv

bridge = CvBridge()
win = False

def show_image(msg):
    global win, bridge
    if not win:
        win = True
        cv.NamedWindow("Cam")
    print "ShowImage"
    cv.ShowImage("Cam", bridge.imgmsg_to_cv(msg,"mono8"))

if __name__ == '__main__':
    try:
        rospy.init_node('CamSub')
        rospy.Subscriber("/cam", Image, show_image)
        rospy.spin()
        cv.DestroyWindow("Cam")
    except rospy.ROSInterruptException:
        pass
