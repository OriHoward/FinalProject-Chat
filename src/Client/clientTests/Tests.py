import multiprocessing
import time
from unittest import TestCase

from Client import Client
from SocketHandler import SocketHandler
from clientTests.SetUp.FakeServer import FakeServer

"""
those tests check both the client and the server and the communication between them
the FakeServer has the same functions as the original one
The GUI is tested as well with error messages
"""


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


    @classmethod
    def tearDownClass(cls) -> None:
        cls.server_thread.terminate()
