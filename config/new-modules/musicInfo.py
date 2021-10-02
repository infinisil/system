import sqlite3
from mpd import MPDClient
import signal
import sys


def signal_handler(signal, frame):
    sys.exit(0)


signal.signal(signal.SIGINT, signal_handler)

client = MPDClient()
client.connect("localhost", 6600)
client.subscribe("rating")
client.subscribe("seek")

database = sqlite3.connect("/home/infinisil/music/beets/beets.db")


def update(event):
    status = client.status()
    state = status["state"]

    if state == "play":
        icon = "<action=mpc -q pause>  </action>"
    elif state == "pause":
        icon = "<action=mpc -q play>  </action>"
    else:
        print("(no music playing)", flush=True)
        return

    song = client.currentsong()
    file = song["file"]

    pathblob = b"/home/infinisil/music/data/"+memoryview(file.encode('utf-8'))

    if event == "message":
        for message in client.readmessages():
            if message["channel"] == "rating":
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
                        client.repeat(1)
                        client.single(1)
                        client.seekcur("+" + str(relativeChange))
                        client.repeat(status["repeat"])
                        client.single(status["single"])
                    elif message["message"] == "backward":
                        client.seekcur("-" + str(relativeChange))

    artistTitle = database.execute("""
        SELECT artist, title
        FROM items
        WHERE path = :path
    """, {"path": pathblob}).fetchone()

    stars = ""
    artist = ""
    title = ""

    if artistTitle is None:
        if "artist" in song:
            artist = song["artist"]
        if "title" in song:
            title = song["title"]
    else:
        artist, title = artistTitle

        cursor = database.execute("""
              SELECT value
              FROM items
              LEFT JOIN item_attributes ON items.id = item_attributes.entity_id
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
                stars += " "
            else:
                stars += " "
            stars += "</action>"

    if artist == "" and title == "":
        name = file
    else:
        if artist == "":
            artist = "(unknown artist)"
        if title == "":
            title = "(unknown title)"
        name = artist + " — " + title

    seekLeft = "<action=mpc sendmessage seek backward>  </action>"
    seekRight = "<action=mpc sendmessage seek forward>  </action>"
    prev = "<action=mpc -q prev> </action>"
    next = "<action=mpc -q next> </action>"

    controls = prev + seekLeft + icon + seekRight + next
    line = name + " " + stars + "  " + controls
    print(line, flush=True)


update("player")

while True:
    events = client.idle("player", "message")
    for event in events:
        update(event)

database.close()
client.close()
