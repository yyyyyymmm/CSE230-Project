# CSE230-Project

## Rhythm Dash: The Haskell Rhythm Game
### Introduction
Rhythm Dash is a rhythm game built with Haskell and the brick library.

### Project Collaborators

| Name             | GitHub Account         |
|------------------|------------------------|
| Yimeng Wang      | yyyyyymmm              |
| Luyao Ma         | luyaomacs              |
| Ruoyang Li       | koichi-domoto          |
| Kerong Xiang     | chloe020624@gmail.com  |

### Gameplay
As the background music plays, notes aligned with the rhythm will flow from the right towards the judgment points on the left. Players interact by pressing corresponding keys at the precise moment to get a high score. Additionally, Players must maintain their health bar, which depletes when hitting traps. The game incorporates special items, like the "Double" power-up, which doubles the score for a limited time. And the health-restoring items, which replenish the player's health bar. Players need to keep their health bar from emptying, or else they will lose the game.

### Core Features
- Synced notes with background music for immersive gameplay.
- Special in-game items to boost scores and replenish health bar.
- Health bar system that requires players to avoid traps.
- Real-time scoring that reflects the accuracy of player interactions.

### Goals
- Display the start screen and game menus.
- Load and play background music.
- Display flowing notes that aligned with the rhythm.
- Implement user interface with multiple keys for hitting notes, getting items and avoiding traps.
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
