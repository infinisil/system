import sqlite3
from mpd import MPDClient
import mpd
import signal
import sys
import os
import time
from contextlib import contextmanager


def signal_handler(signal, frame):
    sys.exit(0)


@contextmanager
def musicInfoPipe():
    filename = os.environ["XDG_RUNTIME_DIR"] + "/musicInfo"
    if os.path.exists(filename):
        os.remove(filename)
    os.mkfifo(filename)
    try:
        yield filename
    finally:
        os.unlink(filename)


signal.signal(signal.SIGINT, signal_handler)


def main(pipe):

    client = MPDClient()
    client.connect("localhost", 6600)
    client.subscribe("rating")
    client.subscribe("seek")
    client.subscribe("toggle")
    client.subscribe("update")
    client.subscribe("playlist")

    database = sqlite3.connect("/home/infinisil/music/beets/beets.db")

    def update(event):
        status = client.status()
        state = status["state"]

        if state == "play":
            icon = "<action=mpc sendmessage toggle 1>  </action>"
        elif state == "pause":
            icon = "<action=mpc sendmessage toggle 1>  </action>"
        elif state == "stop":
            icon = "<action=mpc sendmessage toggle 1>  </action>"

        song = client.currentsong()

        if "file" in song:
            file = song["file"]
            b = b"/home/infinisil/music/data/"
            pathblob = b + memoryview(file.encode('utf-8'))
        else:
            file = None
            pathblob = None

        if event == "message":
            for message in client.readmessages():
                if message["channel"] == "rating" and pathblob is not None:
                    rating = float(message["message"])
                    cursor = database.execute("""
                        INSERT INTO item_attributes (entity_id, key, value)
                        SELECT id, "rating", :rating
                        FROM items
                        WHERE path = :path
                    """, {"path": pathblob, "rating": rating})
                    database.commit()
                elif message["channel"] == "seek":
                    if "duration" in song:
                        relativeChange = float(song["duration"]) / 10
                        if message["message"] == "forward":
                            print("Seeking forward", flush=True)
                            client.repeat(1)
                            client.single(1)
                            client.seekcur("+" + str(relativeChange))
                            # Go back 10 seconds so that
                            # the song can't end immediately
                            client.seekcur("-10.0")
                            client.repeat(status["repeat"])
                            client.single(status["single"])
                        elif message["message"] == "backward":
                            print("Seeking backward", flush=True)
                            client.seekcur("-" + str(relativeChange))
                elif message["channel"] == "toggle":
                    if state == "play":
                        if "duration" in song:
                            print("Pausing (song)", flush=True)
                            client.pause()
                        else:
                            print("Pausing (stream)", flush=True)
                            client.stop()
                    else:
                        print("Pausing", flush=True)
                        client.play()
                elif message["channel"] == "playlist":
                    if message["message"] == "next":
                        print("Next", flush=True)
                        client.next()
                    elif message["message"] == "prev":
                        print("Prev", flush=True)
                        client.previous()
                elif message["channel"] == "update":
                    print("Updating", flush=True)

        songArtist = ""
        if "artist" in song:
            songArtist = song["artist"].strip()

        songTitle = ""
        if "title" in song:
            songTitle = song["title"].strip()

        dbArtist = ""
        dbTitle = ""
        inDb = False
        if pathblob is not None:
            artistTitle = database.execute("""
                SELECT artist, title
                FROM items
                WHERE path = :path
            """, {"path": pathblob}).fetchone()

            if artistTitle is not None:
                dbArtist, dbTitle = artistTitle
                dbArtist = dbArtist.strip()
                dbTitle = dbTitle.strip()
                inDb = True

        if songTitle != "":
            title = songTitle
        elif dbTitle != "":
            title = dbTitle
        else:
            title = ""

        if songArtist != "":
            artist = songArtist
        elif dbArtist != "":
            artist = dbArtist
        elif "-" in title:
            splits = title.split("-", 2)
            artist = splits[0]
            title = splits[1]
        else:
            artist = ""

        if artist == "":
            if title == "":
                if file is None:
                    name = "(unknown)"
                else:
                    name = file
            else:
                name = title
        else:
            if title == "":
                name = artist + " - (unknown)"
            else:
                name = artist + " - " + title

        print("Currently playing: " + name, flush=True)

        stars = ""
        if inDb:
            cursor = database.execute("""
                  SELECT value
                  FROM items
                  LEFT JOIN item_attributes
                  ON items.id = item_attributes.entity_id
                  WHERE item_attributes.key = "rating"
                  AND path = :path
            """, {"path": pathblob})
            row = cursor.fetchone()
            if row is None:
                rating = 0.0
            else:
                rating, = row
                rating = float(rating)

            for i in range(1, 11):
                stars += "<action=mpc sendmessage rating " + str(i) + ">"
                if rating >= i:
                    # If we don't add these spaces,
                    # xmobar displays the stars way too tightly
                    stars += ""
                else:
                    stars += ""
                stars += "</action>"

        seekLeft = "<action=mpc sendmessage seek backward>  </action>"
        seekRight = "<action=mpc sendmessage seek forward>  </action>"
        prev = "<action=mpc sendmessage playlist prev> </action>"
        next = "<action=mpc sendmessage playlist next> </action>"

        controls = prev + seekLeft + icon + seekRight + next
        line = name + "  " + stars + "  " + controls
        print(line, flush=True, file=open(pipe, "w"))

    update("player")

    while True:
        events = client.idle("player", "message")
        for event in events:
            update(event)

    database.close()
    client.close()


with musicInfoPipe() as pipe:
    while True:
        try:
            main(pipe)
        except mpd.base.ConnectionError:
            print("restarting", flush=True)
        except ConnectionRefusedError:
            time.sleep(0.1)
            print("restarting", flush=True)
