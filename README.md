# CSE230-Project

## Rhythm Dash: The Haskell Rhythm Game
### Introduction
Rhythm Dash is an innovative rhythm game built with Haskell and the brick library.

### Gameplay
As the background music plays, notes aligned with the rhythm will flow from the right towards the judgment points on the left. Players interact by pressing corresponding keys at the precise moment, striving for the highest score. The game incorporates special items, like the "Double" power-up, which doubles the score for a limited time. And the health-restoring items, which replenish the player's health bar. Additionally, players must maintain their health bar, which depletes upon hitting traps.


### Core Features
Synced notes with background music for immersive gameplay.
Special in-game items to boost scores and replenish health bar.
Health bar system that requires players to avoid traps.
Real-time scoring that reflects the accuracy of player interactions.

### Goals
Display the start screen and game menus.
Begin playing background music upon game initiation.
Implement a multi-key interface for hitting notes.
Real-time scoring system with immediate feedback.
Results screen showcasing the final score and performance metrics.

### Stretch Goals
Introduce varied note types, such as short and long presses, with combo sequences.
Enhance the result page to display charming hit proportions and longest combo streaks.
Incorporate different difficulty levels and song options.
Develop multiplayer functionality and music importation APIs.

### Implementation Roadmap
UI Development: Utilize the brick library to create the start screen, note columns, note graphics, current score display, hit comments, and the results page.
Game Logic: Integrate music playback, note generation in sync with the rhythm, and score computation based on player actions.
Testing: Perform unit testing for each component to ensure reliability and performance.
Extended Features: Implement stretch goals, refine features, and conduct comprehensive testing.
Finalization: Complete the project with thorough documentation and prepare for the final presentation and demonstration.
