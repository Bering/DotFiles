from libqtile.command_client import CommandClient
c = CommandClient()
print(c.screen.info()["index"])
