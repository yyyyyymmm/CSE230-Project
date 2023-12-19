# CSE230-Project

## Rhythm Dash: The Haskell Rhythm Game
### Introduction
Rhythm Dash is a rhythm game built with Haskell and the brick library. The reference game is MuseDash.

### Project Collaborators

| Name             | GitHub Account         |
|------------------|------------------------|
| Yimeng Wang      | yyyyyymmm              |
| Luyao Ma         | luyaomacs              |
| Ruoyang Li       | koichi-domoto          |
| Kerong Xiang     | chloe020624@gmail.com  |

### Gameplay
As the background music plays, notes aligned with the rhythm will flow from the right towards the judgment points on the left. Players interact by pressing corresponding keys at the precise moment to get a high score. Additionally, Players must maintain their blood level, which depletes when missing hitting. The game incorporates special items, like the "Double" power-up, which doubles the score for a limited time (we call it "bonus time"). And the blood-level-restoring items, which replenish the player's blood level. Players need to keep their blood level from emptying, or else they will lose the game.

### Core Features
- Synced notes with background music.
- Special in-game items to boost scores and replenish blood level.
- Real-time scoring that reflects the accuracy of player interactions.

### Goals
- Display the start screen and game menus.
- Load and play background music.
- Display flowing notes that aligned with the rhythm.
- Implement user interface with multiple keys for hitting notes and getting special items.
- Implement real-time scoring system with immediate feedback.
- Show final scores and other performance metrics in the result page.

### Further Goals
- Introduce varied note types, such as short and long presses, with combo sequences.
- Incorporate different difficulty levels and song options.
- Develop local multiplayer mode or networked multiplayer mode.

### Implementation Roadmap
- UI Development: Use the brick library to create the start screen, note rows, note graphics, current score display, hit feedback, and the results page.
- Game Logic: Integrate music playback, note generation in sync with the rhythm, and score computation based on player actions.
- Testing: Perform unit testing for each component to ensure reliability and performance.
- Extended Features: Implement further goals, refine features, and conduct comprehensive testing.
- Finalization: Complete the project with thorough documentation and prepare for the final presentation and demonstration.

## Updates

### Architecture of Application

- `Sound`: Engaging background music that accompanies the game-play experience.
- `Graphics`: Dynamic notes appearing in sync with the rhythm.
- `Interaction`: The player's score dynamically updates as they press corresponding keys on the keyboard.

### Challenges and Solutions

So far, we have set up the basic framework of the whole program. With the background music playing, notes that are synchronized with the rhythm flow from the right to the judgment point on the left. Players strive to achieve high scores by accurately pressing the corresponding keys at the right moment.

Though the challenge is that we need to accelerate the pace of our progress, we still think we are able to achieve the basic functionality we envisioned earlier. In the upcoming time, we will continue working on completing the content of our project to achieve our goals.

Next step, we need to :

1. Showcase flowing notes that are synchronized with the rhythm.
2. Develop a user interface with multiple keys for hitting notes, acquiring items, and avoiding traps.
3. Implement a real-time scoring system that provides immediate feedback.
4. Display final scores and other performance metrics on the results page.

### Do we expect to meet our goals until the deadline?

- Yes

## Game Logic Implementation

```
.
|-- app
|   `-- Main.hs
|-- assets
|   |-- MusicChoice.txt
|   `-- bestResult.txt
|-- src
|   |-- GameUI.hs
|   |-- GameUtils.hs
|   |-- MusicUI.hs
|   `-- StartUI.hs
`-- test
    |-- Spec.hs
    `-- TestUtil.hs
```

![mushdash](./img/MushDash.png)


### Start Menu

The implementation of start menu is in [StartUI.hs](https://github.com/yyyyyymmm/CSE230-Project/blob/main/src/StartUI.hs).

![startmenu](./img/startmenu.png)

### Choose Music

The implementation of choosing music is in [MusicUI.hs](https://github.com/yyyyyymmm/CSE230-Project/blob/main/src/MusicUI.hs).
![startmenu](./img/choosemusic.png)

### Game Playing

The main part of the game UI is in [GameUI.hs](https://github.com/yyyyyymmm/CSE230-Project/blob/main/src/GameUI.hs), underlying game logit is in [GameUtils.hs](https://github.com/yyyyyymmm/CSE230-Project/blob/main/src/GameUtils.hs).
![startmenu](./img/GameUI.png)

## Source Acknowledgment
We learned the basic framework of how to define App, how to set up a channel to handle Tick events and how to handle keyboard events from the implementation of a dino game at [https://github.com/arcticmatt/dino-brick/tree/dino](https://github.com/arcticmatt/dino-brick/tree/dino) and the implementation of a snake game at [https://github.com/samtay/tetris/tree/master](https://github.com/samtay/tetris/tree/master). We also learned the basic commands to draw UI. However, all the game logic and the UI are implemented by ourselves.

We discovered an existing implementation of another rhythm game at [https://github.com/erichanxin/CSE-230-Project-Rhythm-Game](https://github.com/erichanxin/CSE-230-Project-Rhythm-Game). The rhythm game we implemented is an imitation for the game MuseDash, while their rhythm game is similar to the game Rhythm Master. There are some common things between the rhythm games but more differences. During the implementation, we were inspired by how they record the notes and the shift of the time and position of notes. We referred to these ideas but made our own implementation. The vast majority of the code is still developed by ourselves.

### Feature Extensions

1. Start Menu: Designed the start interface with Start Game, Choose Music and Quit options.

2. Music Selection: Designed the music selection interface. This interface allows players to select from different music options.

3. Music Playing: Utilized ‘mpv’ to implement music playing which can be used both on Linux and MacOS.

4. Notes Movement: Developed notes moving from left to right according to the beat of music. The notes will disappear after reaching the hit points.

5. Hit Judgement and Score: Developed the notes hit judgment system with three different levels: Perfect for 2 points, Good for 1 points, Miss for 0 points.

6. Hp System: Designed the hp system to record the hp during the game. The game ends when hp is 0.

7. Prop System: Designed the prop system. The props have different effects of healing or entering bonus time. The healing will +2 hp. The bonus time will earn x2 scores.

8. Best Results: Implemented a feature to display the best results achieved by the player. This includes high scores and max combos.







