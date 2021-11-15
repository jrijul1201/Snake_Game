import * as firebase from "firebase/app";
import "firebase/firebase-auth";
import "firebase/firebase-firestore";

import { Elm } from "./Main.elm";
console.log("AIzaSyAbTT8R6UNSgPTd-PY7k-2WZlCgVoZHDG8" !== undefined);

const firebaseConfig = {
    apiKey: "AIzaSyAbTT8R6UNSgPTd-PY7k-2WZlCgVoZHDG8",
    authDomain: "snake-game-d6f11.firebaseapp.com",
    projectId: "snake-game-d6f11",
    storageBucket: "snake-game-d6f11.appspot.com",
    messagingSenderId: "794201411451",
    appId: "1:794201411451:web:c9b616822b2efcf9b377de",
    measurementId: "G-YWL9FBLBB6"
};

firebase.initializeApp(firebaseConfig);

const provider = new firebase.auth.GoogleAuthProvider();
const db = firebase.firestore();
const app = Elm.Main.init({
    node: document.getElementById("root")
});

var HIGHSCORE_EASY = 0;
var HIGHSCORE_MEDIUM = 0;
var HIGHSCORE_HARD = 0;

updateLeaderboard();

app.ports.signIn.subscribe(() => {
    console.log("LogIn called");
    firebase
        .auth()
        .signInWithPopup(provider)
        .then(result => {
            console.log("AuthState will handle this")
        })
        .catch(error => {
            app.ports.signInError.send({
                code: error.code,
                message: error.message
            });
        });
});

app.ports.signOut.subscribe(() => {
    console.log("LogOut called");
    firebase.auth().signOut();
});


//  Observer on user info
firebase.auth().onAuthStateChanged(user => {
    console.log("called");
    if (user) {
        console.log("User is already logged-in");
        // Set up listened on new messages
        db.collection(`users`).doc(user.uid).onSnapshot(data => {
            let highscorefromdb_easy = highscoreLevel(data, "EASY");
            let highscorefromdb_medium = highscoreLevel(data, "MEDIUM");
            let highscorefromdb_hard = highscoreLevel(data, "HARD");
            user
                .getIdToken()
                .then(idToken => {
                    app.ports.signInInfo.send({
                        name: user.displayName,
                        uid: user.uid,
                        highScore: {
                            easy: highscorefromdb_easy,
                            medium: highscorefromdb_medium,
                            hard: highscorefromdb_hard
                        }
                    });
                })
                .catch(error => {
                    console.log("Error when retrieving cached user");
                    console.log(error);
                });

            updateLeaderboard();
        });
    }
});

app.ports.saveHighScore.subscribe(data => {
    console.log(`Saving highscore to database`);
    if (data && data.uid && (data.easy > HIGHSCORE_EASY || data.medium > HIGHSCORE_MEDIUM || data.hard > HIGHSCORE_HARD)) {
        db.collection(`users`).doc(data.uid)
            .set({
                display_name: data.displayname,
                high_score_easy: data.easy,
                high_score_medium: data.medium,
                high_score_hard: data.hard
            })
            .catch(error => {
                app.ports.signInError.send({
                    code: error.code,
                    message: error.message
                });
            });
    }
});

//Prevents scrolling using arrow keys
var keys = {};
window.addEventListener("keydown",
    function(e) {
        keys[e.keyCode] = true;
        switch (e.keyCode) {
            case 37:
            case 39:
            case 38:
            case 40:
            case 32:
                e.preventDefault();
                break;
            default:
                break;
        }
    }, false);



function sortFunction(a, b) {
    if (a[1] === b[1]) {
        return 0;
    } else {
        return (a[1] > b[1]) ? -1 : 1;
    }
}

function updateLeaderboard() {
    var scoreboard_easy = [];
    var scoreboard_medium = [];
    var scoreboard_hard = [];
    db.collection("users").get().then((querySnapshot) => {
        querySnapshot.forEach((doc) => {
            var row = [];
            row.push(doc.data().display_name);
            row.push(doc.data().high_score_easy);
            scoreboard_easy.push(row);

        });
        console.log("Scoreboard created!");
        scoreboard_easy.sort(sortFunction);
        console.log("Easy:", scoreboard_easy)

        querySnapshot.forEach((doc) => {
            var row = [];
            row.push(doc.data().display_name);
            row.push(doc.data().high_score_medium);
            scoreboard_medium.push(row);

        });
        scoreboard_medium.sort(sortFunction);
        console.log("Scoreboard created!");
        console.log("Medium:", scoreboard_medium)

        querySnapshot.forEach((doc) => {
            var row = [];
            row.push(doc.data().display_name);
            row.push(doc.data().high_score_hard);
            scoreboard_hard.push(row);

        });
        console.log("Scoreboard created!");
        scoreboard_hard.sort(sortFunction);
        console.log("Hard:", scoreboard_hard)

        app.ports.receiveLeaderboards.send({
            easy: {
                first: {
                    name: scoreboard_easy[0][0],
                    highScore: scoreboard_easy[0][1]
                },
                second: {
                    name: scoreboard_easy[1][0],
                    highScore: scoreboard_easy[1][1]
                },
                third: {
                    name: scoreboard_easy[2][0],
                    highScore: scoreboard_easy[2][1]
                }
            },
            medium: {
                first: {
                    name: scoreboard_medium[0][0],
                    highScore: scoreboard_medium[0][1]
                },
                second: {
                    name: scoreboard_medium[1][0],
                    highScore: scoreboard_medium[1][1]
                },
                third: {
                    name: scoreboard_medium[2][0],
                    highScore: scoreboard_medium[2][1]
                }
            },
            hard: {
                first: {
                    name: scoreboard_hard[0][0],
                    highScore: scoreboard_hard[0][1]
                },
                second: {
                    name: scoreboard_hard[1][0],
                    highScore: scoreboard_hard[1][1]
                },
                third: {
                    name: scoreboard_hard[2][0],
                    highScore: scoreboard_hard[2][1]
                }
            }
        });
    });
}


function highscoreLevel(data, level) {
    let highscorefromdb = 0;
    if (data && data.data()) {
        if (level == "EASY") {
            if (data.data().high_score_easy) {
                highscorefromdb = data.data().high_score_easy;
                console.log("Received new snapshot", highscorefromdb);
                HIGHSCORE_EASY = Math.max(HIGHSCORE_EASY, highscorefromdb);
            }
        } else if (level == "MEDIUM") {
            if (data.data().high_score_medium) {
                highscorefromdb = data.data().high_score_medium;
                console.log("Received new snapshot", highscorefromdb);
                HIGHSCORE_MEDIUM = Math.max(HIGHSCORE_MEDIUM, highscorefromdb);
            }
        } else {
            if (data.data().high_score_hard) {
                highscorefromdb = data.data().high_score_hard;
                console.log("Received new snapshot", highscorefromdb);
                HIGHSCORE_HARD = Math.max(HIGHSCORE_HARD, highscorefromdb);
            }
        }
    }
    return highscorefromdb;
}