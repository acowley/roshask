#!/usr/bin/env python
import roslib; roslib.load_manifest('CamSub')
import rospy
from sensor_msgs.msg import Image
from cv_bridge import CvBridge
import cv

bridge = CvBridge()

def show_image(msg):
    global bridge
    #cv.ShowImage("Cam", bridge.imgmsg_to_cv(msg,"mono8"))
    cv.ShowImage("Cam", bridge.imgmsg_to_cv(msg,))

if __name__ == '__main__':
    try:
        rospy.init_node('CamSub')
        cv.NamedWindow("Cam")
        cv.StartWindowThread()
        # Usage: rosrun CamSub CamSub.py _topic:=/foo
        topic = rospy.get_param("~topic","/diff")
        print "Subscribing to "+topic
        rospy.Subscriber(topic, Image, show_image)
        rospy.spin()
        cv.DestroyWindow("Cam")
    except rospy.ROSInterruptException:
        pass
