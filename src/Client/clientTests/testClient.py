import multiprocessing
import threading
import time
from unittest import TestCase

from SocketHandler import SocketHandler
from Client import Client
from clientTests.SetUp.FakeServer import FakeServer


class TestClient(TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.server = FakeServer()
        cls.first_user = Client("Ori")
        cls.second_user = Client("Avi")
        cls.server_thread = multiprocessing.Process(target=cls.server.start, daemon=True)
        cls.server_thread.start()

    def test_connect(cls):
        cls.first_user.connect()
        time.sleep(0.1)
        cls.assertTrue(cls.first_user.is_connected())
        cls.first_user.disconnect()
        time.sleep(1)

    def test_disconnect(cls):
        cls.first_user.connect()
        cls.first_user.disconnect()
        time.sleep(0.1)
        cls.assertFalse(cls.first_user.is_connected())
        time.sleep(1)

    def test_get_users(cls):
        cls.first_user.connect()
        cls.second_user.connect()
        time.sleep(0.2)
        cls.first_user.get_users()
        list_of_clients = SocketHandler.get_msg(cls.first_user.tcp_socket)
        expected_list = "Ori,Avi"
        cls.assertEqual(expected_list, list_of_clients)
        time.sleep(1)

    def test_send_private_msg(cls):
        cls.first_user.connect()
        cls.second_user.connect()
        time.sleep(0.1)
        cls.first_user.send_private_msg("hello!", "Avi")
        time.sleep(0.1)
        first_user_msg_back = SocketHandler.get_msg(cls.first_user.tcp_socket)
        expected_first_user_msg = "to Avi: hello!"
        second_user_msg_back = SocketHandler.get_msg(cls.second_user.tcp_socket)
        time.sleep(0.1)
        expected_second_user_msg = "from Ori: hello!"
        cls.assertEqual(expected_first_user_msg, first_user_msg_back)
        cls.assertEqual(expected_second_user_msg, second_user_msg_back)
        time.sleep(1)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.server_thread.terminate()
