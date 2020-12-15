// js and cs files
import './assets/main.css';
import './assets/howler.core';
import './assets/simptip.min.css';
import WakeLock from './assets/WakeLock';

// images and files
import smh from './assets/img/smh.png';
import iosShareIcon from './assets/img/ios_share_icon.png';
import tadamp3File from './assets/sounds/tada.mp3';
import tadawavFile from './assets/sounds/tada.wav';
import tickFile from './assets/sounds/tick.mp3';
import whistleFile from './assets/sounds/whistle.mp3';

// other imports
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

// for the window to s p e a k
let synth = window.speechSynthesis;
let wakeLock = new WakeLock();

// iOS "Add to home screen" popup
// Detects if device is on iOS 
const isIos = () => {
    const userAgent = window.navigator.userAgent.toLowerCase();
    console.log(userAgent)
    return /iphone|ipad|ipod/.test(userAgent);
}

// Detects if device is in standalone mode
const isInStandaloneMode = () => ('standalone' in window.navigator) && (window.navigator.standalone);
// Checks if should display install popup notification:
let showIosInstall = isIos() && !isInStandaloneMode();


// init sounds

const whistle = new Howl({
    volume: 0.8,
    src: [whistleFile]
});

const tada = new Howl({
    volume: 0.8,
    src: [tadamp3File, tadawavFile]
});

const tick = new Howl({
    volume: 1,
    src: [tickFile]
});

var storedConfig = localStorage.getItem('config');
console.log("Retrieved state: ", storedConfig);

// somehow using `process.env.npm_package_version doesn't work
const VERSION = process.env.ELM_APP_PRODUCT_VERSION || null;
console.log('version:', VERSION);


var app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: {
        windowSize: {
            height: window.innerHeight,
            width: window.innerWidth
        },
        storedConfig: JSON.parse(storedConfig),
        showIosInstall: showIosInstall,
        images: {
            smhSrc: smh,
            iosShareIconSrc: iosShareIcon
        },
        version: VERSION
    }
});

app.ports.storeConfig.subscribe(config => {
    let configJson = JSON.stringify(config);
    localStorage.setItem('config', configJson);
    console.log("Saved state: ", configJson);
    // the actual process is too fast so I set a timeout to make it seem more realistic LOL
    setTimeout(() => app.ports.storeConfigSuccess.send(null), 250);
});


app.ports.playSound.subscribe(sound => {
    switch (sound) {
        case 'whistle':
            whistle.play();
            break;
        case 'tada':
            tada.play();
            break;
        case 'tick':
            tick.play();
            break;
    }
});

app.ports.workoutStatus.subscribe(status => {
    switch (status) {
        case 'start':
            wakeLock.start();
            break;
        case 'end':
            wakeLock.end();
            break;
    }
});

app.ports.speak.subscribe(value => {
    let speaker = new SpeechSynthesisUtterance(value);
    let voices = synth.getVoices()
    console.log(voices)
    // Google US English is the best, but we default to Samantha. If none, it will default to Alex
    let voice = voices.filter(function (voice) { return voice.name == 'Google US English' || voice.name == 'Samantha' })[0];
    console.log(voice)
    speaker.voice = voice
    synth.speak(speaker);
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
