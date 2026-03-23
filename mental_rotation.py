"""
Mental Rotation Experiment Simulator
Based on Shepard & Metzler (1971) paradigm.

Tests whether reaction time increases and accuracy decreases
with greater angular disparity (0°, 90°, 180°, 270°).

Within-subjects design: 32 trials (4 angles × 8 trials each).

Run: python mental_rotation.py
Dependencies: pygame-ce (pip install pygame-ce)
"""

import pygame
import math
import random
import csv
import time
import os
from datetime import datetime

# ─── Constants ───────────────────────────────────────────────────────────────
SCREEN_WIDTH = 1024
SCREEN_HEIGHT = 700
FPS = 60
BG_COLOR = (240, 240, 240)
TEXT_COLOR = (30, 30, 30)
ACCENT_COLOR = (50, 100, 200)
COUNTDOWN_COLOR = (200, 50, 50)
WHITE = (255, 255, 255)
LIGHT_GRAY = (220, 220, 220)
BUTTON_COLOR = (50, 100, 200)
BUTTON_HOVER = (70, 130, 240)
BUTTON_TEXT = (255, 255, 255)

ROTATION_ANGLES = [0, 90, 180, 270]
TRIALS_PER_ANGLE = 8  # 4 same + 4 different
SAME_PER_ANGLE = 4
DIFF_PER_ANGLE = 4
TOTAL_TRIALS = len(ROTATION_ANGLES) * TRIALS_PER_ANGLE  # 32
PRACTICE_TRIALS = 5
COUNTDOWN_SECS = 3

RESPONSE_KEY_SAME = pygame.K_s      # 'S' for same
RESPONSE_KEY_DIFF = pygame.K_d      # 'D' for different

# ─── Isometric 3D Block Rendering ────────────────────────────────────────────

# Isometric projection helpers
ISO_SCALE = 18
ISO_ANGLE = math.pi / 6  # 30 degrees

def iso_project(x, y, z):
    """Project 3D coordinates to 2D isometric view."""
    px = (x - z) * math.cos(ISO_ANGLE) * ISO_SCALE
    py = (x + z) * math.sin(ISO_ANGLE) * ISO_SCALE - y * ISO_SCALE
    return px, py


def draw_cube(surface, x, y, z, offset_x, offset_y, color, outline=(60, 60, 60)):
    """Draw a single isometric cube at grid position (x, y, z)."""
    # 8 corners of the cube
    corners_3d = [
        (x, y, z), (x+1, y, z), (x+1, y, z+1), (x, y, z+1),
        (x, y+1, z), (x+1, y+1, z), (x+1, y+1, z+1), (x, y+1, z+1),
    ]
    corners_2d = []
    for cx, cy, cz in corners_3d:
        px, py = iso_project(cx, cy, cz)
        corners_2d.append((px + offset_x, py + offset_y))

    # Faces (top, left, right) with shading
    r, g, b = color
    top_color = (min(r + 40, 255), min(g + 40, 255), min(b + 40, 255))
    left_color = (max(r - 30, 0), max(g - 30, 0), max(b - 30, 0))
    right_color = (max(r - 60, 0), max(g - 60, 0), max(b - 60, 0))

    # Top face: 0,1,5,4 -> projected: corners 4,5,1,0 (y+1 is bottom visually)
    top = [corners_2d[0], corners_2d[1], corners_2d[2], corners_2d[3]]
    pygame.draw.polygon(surface, top_color, top)
    pygame.draw.polygon(surface, outline, top, 1)

    # Front-left face
    left = [corners_2d[0], corners_2d[3], corners_2d[7], corners_2d[4]]
    pygame.draw.polygon(surface, left_color, left)
    pygame.draw.polygon(surface, outline, left, 1)

    # Front-right face
    right = [corners_2d[1], corners_2d[0], corners_2d[4], corners_2d[5]]
    pygame.draw.polygon(surface, right_color, right)
    pygame.draw.polygon(surface, outline, right, 1)


# ─── Block Object Definitions ────────────────────────────────────────────────
# Shepard-Metzler style: arm-like 3D structures made of cubes

# 10-cube objects (2 variants)
OBJECTS_10 = [
    # Object A: L-shape with vertical extension
    [(0,0,0),(1,0,0),(2,0,0),(2,1,0),(2,2,0),(2,2,1),(2,2,2),(2,2,3),(2,3,3),(2,4,3)],
    # Object B: Z-shape with vertical extension
    [(0,0,0),(0,1,0),(0,2,0),(1,2,0),(2,2,0),(2,2,1),(2,2,2),(3,2,2),(3,3,2),(3,4,2)],
]

# 8-cube objects (2 variants)
OBJECTS_8 = [
    # Object C: T-shape variant
    [(0,0,0),(1,0,0),(2,0,0),(2,1,0),(2,2,0),(2,2,1),(2,2,2),(3,2,2)],
    # Object D: S-shape variant
    [(0,0,0),(0,1,0),(1,1,0),(2,1,0),(2,1,1),(2,1,2),(2,2,2),(2,3,2)],
]

ALL_OBJECTS = OBJECTS_10 + OBJECTS_8
OBJECT_NAMES = ["10cube_A", "10cube_B", "8cube_C", "8cube_D"]
OBJECT_COLORS = [
    (100, 160, 220),  # blue
    (100, 160, 220),
    (100, 160, 220),
    (100, 160, 220),
]

# Distractor pairing: 10-cube distractor for 10-cube, 8-cube for 8-cube
DISTRACTOR_MAP = {0: 1, 1: 0, 2: 3, 3: 2}


def rotate_object_y(cubes, angle_deg):
    """Rotate a set of cube positions around Y axis by angle_deg."""
    angle = math.radians(angle_deg)
    cos_a = round(math.cos(angle))
    sin_a = round(math.sin(angle))

    # Center the object
    cx = sum(c[0] for c in cubes) / len(cubes)
    cz = sum(c[2] for c in cubes) / len(cubes)

    rotated = []
    for x, y, z in cubes:
        dx, dz = x - cx, z - cz
        nx = dx * cos_a - dz * sin_a
        nz = dx * sin_a + dz * cos_a
        rotated.append((round(nx + cx, 1), y, round(nz + cz, 1)))

    # Normalize to integer grid
    min_x = min(c[0] for c in rotated)
    min_z = min(c[2] for c in rotated)
    return [(round(x - min_x), y, round(z - min_z)) for x, y, z in rotated]


def render_object(cubes, color, size=200):
    """Render a 3D block object to a pygame Surface."""
    surface = pygame.Surface((size, size), pygame.SRCALPHA)

    # Sort cubes for painter's algorithm (back to front)
    sorted_cubes = sorted(cubes, key=lambda c: (-c[1], -c[0] - c[2]))

    # Find bounding box in projected space to center
    all_proj = []
    for x, y, z in cubes:
        for dx in (0, 1):
            for dy in (0, 1):
                for dz in (0, 1):
                    px, py = iso_project(x+dx, y+dy, z+dz)
                    all_proj.append((px, py))

    min_px = min(p[0] for p in all_proj)
    max_px = max(p[0] for p in all_proj)
    min_py = min(p[1] for p in all_proj)
    max_py = max(p[1] for p in all_proj)

    obj_w = max_px - min_px
    obj_h = max_py - min_py
    offset_x = (size - obj_w) / 2 - min_px
    offset_y = (size - obj_h) / 2 - min_py

    for x, y, z in sorted_cubes:
        draw_cube(surface, x, y, z, offset_x, offset_y, color)

    return surface


# ─── Trial Generation ────────────────────────────────────────────────────────

def generate_trials():
    """Generate 32 randomized trials: 4 angles × (4 same + 4 different)."""
    trials = []

    for angle in ROTATION_ANGLES:
        # 4 same trials: pick objects, rotate comparison
        same_objects = [0, 1, 2, 3]  # Use all 4 objects for same trials
        for i in range(SAME_PER_ANGLE):
            obj_idx = same_objects[i]
            trials.append({
                "angle": angle,
                "trial_type": "same",
                "ref_obj": obj_idx,
                "comp_obj": obj_idx,
                "object_name": OBJECT_NAMES[obj_idx],
                "correct_response": "same",
            })

        # 4 different trials: use distractor objects
        diff_objects = [0, 1, 2, 3]
        for i in range(DIFF_PER_ANGLE):
            obj_idx = diff_objects[i]
            dist_idx = DISTRACTOR_MAP[obj_idx]
            trials.append({
                "angle": angle,
                "trial_type": "different",
                "ref_obj": obj_idx,
                "comp_obj": dist_idx,
                "object_name": OBJECT_NAMES[obj_idx],
                "correct_response": "different",
            })

    random.shuffle(trials)
    return trials


def generate_practice_trials():
    """Generate 5 practice trials with mixed angles and types."""
    practice = []
    configs = [
        (0, "same", 0, 0),
        (90, "different", 1, 0),
        (180, "same", 2, 2),
        (270, "different", 3, 2),
        (90, "same", 1, 1),
    ]
    for angle, ttype, ref, comp in configs:
        practice.append({
            "angle": angle,
            "trial_type": ttype,
            "ref_obj": ref,
            "comp_obj": comp,
            "object_name": OBJECT_NAMES[ref],
            "correct_response": ttype,
        })
    return practice


# ─── Experiment Application ──────────────────────────────────────────────────

class Experiment:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Mental Rotation Experiment")
        self.clock = pygame.time.Clock()
        self.font_large = pygame.font.SysFont("Arial", 42, bold=True)
        self.font_medium = pygame.font.SysFont("Arial", 28)
        self.font_small = pygame.font.SysFont("Arial", 20)
        self.font_countdown = pygame.font.SysFont("Arial", 80, bold=True)

        self.participant_id = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.results = []
        self.running = True

        # Pre-render all object surfaces at all angles
        self.object_cache = {}
        for obj_idx, cubes in enumerate(ALL_OBJECTS):
            for angle in [0, 90, 180, 270]:
                rotated = rotate_object_y(cubes, angle)
                surf = render_object(rotated, OBJECT_COLORS[obj_idx], 250)
                self.object_cache[(obj_idx, angle)] = surf

    def draw_text_centered(self, text, font, color, y):
        rendered = font.render(text, True, color)
        rect = rendered.get_rect(center=(SCREEN_WIDTH // 2, y))
        self.screen.blit(rendered, rect)

    def draw_button(self, text, rect, mouse_pos):
        hovered = rect.collidepoint(mouse_pos)
        color = BUTTON_HOVER if hovered else BUTTON_COLOR
        pygame.draw.rect(self.screen, color, rect, border_radius=8)
        txt = self.font_small.render(text, True, BUTTON_TEXT)
        txt_rect = txt.get_rect(center=rect.center)
        self.screen.blit(txt, txt_rect)
        return hovered

    def instructions_screen(self):
        """Show task instructions."""
        lines = [
            "In this task, you will see two 3D block objects side by side.",
            "",
            "Your job is to determine if the two objects are the",
            "SAME shape (possibly rotated) or DIFFERENT shapes.",
            "",
            "Press 'S' if the objects are the SAME.",
            "Press 'D' if the objects are DIFFERENT.",
            "",
            "Each trial begins with a 3-second countdown.",
            "Respond as quickly and accurately as possible.",
            "No feedback will be provided.",
            "",
            "You will start with 5 practice trials.",
        ]

        while self.running:
            mouse_pos = pygame.mouse.get_pos()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                    return False
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if start_rect.collidepoint(mouse_pos):
                        return True

            self.screen.fill(BG_COLOR)
            self.draw_text_centered("Instructions", self.font_large, TEXT_COLOR, 40)

            for i, line in enumerate(lines):
                if line:
                    self.draw_text_centered(line, self.font_small, TEXT_COLOR, 110 + i * 32)

            start_rect = pygame.Rect(SCREEN_WIDTH // 2 - 100, 560, 200, 50)
            self.draw_button("Start Practice", start_rect, mouse_pos)

            # Key reminders
            pygame.draw.rect(self.screen, WHITE, pygame.Rect(SCREEN_WIDTH//2 - 150, 490, 300, 50), border_radius=8)
            self.draw_text_centered("S = Same    |    D = Different", self.font_medium, ACCENT_COLOR, 515)

            pygame.display.flip()
            self.clock.tick(FPS)
        return False

    def countdown(self):
        """Display 3-second countdown before a trial."""
        for sec in range(COUNTDOWN_SECS, 0, -1):
            start = time.time()
            while time.time() - start < 1.0:
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        self.running = False
                        return False
                self.screen.fill(BG_COLOR)
                self.draw_text_centered(str(sec), self.font_countdown, COUNTDOWN_COLOR, SCREEN_HEIGHT // 2 - 20)
                self.draw_text_centered("Get ready...", self.font_small, TEXT_COLOR, SCREEN_HEIGHT // 2 + 60)
                pygame.display.flip()
                self.clock.tick(FPS)
        return True

    def run_trial(self, trial, trial_num, total, is_practice=False):
        """Run a single trial. Returns (response, rt_ms, correct) or None if quit."""
        # Countdown
        if not self.countdown():
            return None

        # Get stimuli
        ref_surface = self.object_cache[(trial["ref_obj"], 0)]
        comp_surface = self.object_cache[(trial["comp_obj"], trial["angle"])]

        # Display stimuli and wait for response
        stimulus_onset = time.perf_counter()
        response = None

        while self.running and response is None:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                    return None
                if event.type == pygame.KEYDOWN:
                    if event.key == RESPONSE_KEY_SAME:
                        response = "same"
                    elif event.key == RESPONSE_KEY_DIFF:
                        response = "different"

            self.screen.fill(BG_COLOR)

            # Header
            label = "Practice" if is_practice else "Trial"
            self.draw_text_centered(
                f"{label} {trial_num}/{total}", self.font_small, (120, 120, 120), 25
            )

            # Draw reference (left)
            ref_x = SCREEN_WIDTH // 4 - 125
            ref_y = SCREEN_HEIGHT // 2 - 125
            pygame.draw.rect(self.screen, WHITE, (ref_x - 10, ref_y - 10, 270, 270), border_radius=10)
            self.screen.blit(ref_surface, (ref_x, ref_y))
            txt = self.font_small.render("Reference", True, TEXT_COLOR)
            self.screen.blit(txt, (ref_x + 90, ref_y + 265))

            # Rotation angle label
            angle_txt = f"Rotated {trial['angle']}°" if trial['angle'] > 0 else "0° (no rotation)"

            # Draw comparison (right)
            comp_x = 3 * SCREEN_WIDTH // 4 - 125
            comp_y = SCREEN_HEIGHT // 2 - 125
            pygame.draw.rect(self.screen, WHITE, (comp_x - 10, comp_y - 10, 270, 270), border_radius=10)
            self.screen.blit(comp_surface, (comp_x, comp_y))
            txt = self.font_small.render("Comparison", True, TEXT_COLOR)
            self.screen.blit(txt, (comp_x + 80, comp_y + 265))

            # Key reminders at bottom
            self.draw_text_centered("S = Same    |    D = Different", self.font_small, (150, 150, 150), SCREEN_HEIGHT - 40)

            pygame.display.flip()
            self.clock.tick(FPS)

        if response is None:
            return None

        rt_ms = (time.perf_counter() - stimulus_onset) * 1000
        correct = 1 if response == trial["correct_response"] else 0

        return response, rt_ms, correct

    def transition_screen(self, title, subtitle):
        """Show a transition/message screen with continue button."""
        while self.running:
            mouse_pos = pygame.mouse.get_pos()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                    return False
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if btn_rect.collidepoint(mouse_pos):
                        return True

            self.screen.fill(BG_COLOR)
            self.draw_text_centered(title, self.font_large, TEXT_COLOR, SCREEN_HEIGHT // 2 - 60)
            self.draw_text_centered(subtitle, self.font_medium, (100, 100, 100), SCREEN_HEIGHT // 2)

            btn_rect = pygame.Rect(SCREEN_WIDTH // 2 - 80, SCREEN_HEIGHT // 2 + 60, 160, 45)
            self.draw_button("Continue", btn_rect, mouse_pos)

            pygame.display.flip()
            self.clock.tick(FPS)
        return False

    def results_screen(self):
        """Show summary results after the experiment."""
        # Compute summary stats
        by_angle = {}
        for r in self.results:
            a = r["angle"]
            if a not in by_angle:
                by_angle[a] = {"rts": [], "accs": []}
            by_angle[a]["rts"].append(r["rt_ms"])
            by_angle[a]["accs"].append(r["correct"])

        while self.running:
            mouse_pos = pygame.mouse.get_pos()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                    return
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if quit_rect.collidepoint(mouse_pos):
                        self.running = False
                        return

            self.screen.fill(BG_COLOR)
            self.draw_text_centered("Experiment Complete!", self.font_large, TEXT_COLOR, 40)
            self.draw_text_centered("Thank you for participating.", self.font_medium, (100,100,100), 85)

            # Results table
            headers = ["Angle", "Mean RT (ms)", "Accuracy"]
            col_x = [SCREEN_WIDTH//2 - 200, SCREEN_WIDTH//2 - 30, SCREEN_WIDTH//2 + 140]
            y_start = 150

            for i, h in enumerate(headers):
                txt = self.font_medium.render(h, True, ACCENT_COLOR)
                self.screen.blit(txt, (col_x[i], y_start))

            pygame.draw.line(self.screen, (180,180,180), (col_x[0], y_start+35), (col_x[2]+120, y_start+35), 2)

            for j, angle in enumerate(ROTATION_ANGLES):
                y = y_start + 50 + j * 40
                data = by_angle.get(angle, {"rts": [0], "accs": [0]})
                mean_rt = sum(data["rts"]) / len(data["rts"])
                mean_acc = sum(data["accs"]) / len(data["accs"])

                vals = [f"{angle}°", f"{mean_rt:.0f}", f"{mean_acc:.2%}"]
                for i, v in enumerate(vals):
                    txt = self.font_medium.render(v, True, TEXT_COLOR)
                    self.screen.blit(txt, (col_x[i], y))

            # Overall
            y = y_start + 50 + 4 * 40 + 10
            pygame.draw.line(self.screen, (180,180,180), (col_x[0], y-5), (col_x[2]+120, y-5), 2)
            all_rts = [r["rt_ms"] for r in self.results]
            all_accs = [r["correct"] for r in self.results]
            overall_rt = sum(all_rts) / len(all_rts) if all_rts else 0
            overall_acc = sum(all_accs) / len(all_accs) if all_accs else 0
            vals = ["Overall", f"{overall_rt:.0f}", f"{overall_acc:.2%}"]
            for i, v in enumerate(vals):
                txt = self.font_medium.render(v, True, TEXT_COLOR)
                self.screen.blit(txt, (col_x[i], y))

            # Data saved message
            self.draw_text_centered(
                f"Data saved to: {self.csv_path}",
                self.font_small, (100, 150, 100), SCREEN_HEIGHT - 100
            )

            quit_rect = pygame.Rect(SCREEN_WIDTH // 2 - 60, SCREEN_HEIGHT - 60, 120, 40)
            self.draw_button("Quit", quit_rect, mouse_pos)

            pygame.display.flip()
            self.clock.tick(FPS)

    def save_results(self):
        """Export trial data to CSV."""
        os.makedirs("data", exist_ok=True)
        self.csv_path = os.path.join("data", f"participant_{self.participant_id}.csv")

        fieldnames = [
            "participant_id",
            "trial_num", "phase", "rotation_angle", "trial_type",
            "ref_object", "comp_object", "object_identity",
            "response", "correct_response", "correct", "rt_ms"
        ]

        with open(self.csv_path, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for r in self.results:
                writer.writerow({
                    "participant_id": self.participant_id,
                    "trial_num": r["trial_num"],
                    "phase": r["phase"],
                    "rotation_angle": r["angle"],
                    "trial_type": r["trial_type"],
                    "ref_object": OBJECT_NAMES[r["ref_obj"]],
                    "comp_object": OBJECT_NAMES[r["comp_obj"]],
                    "object_identity": r["object_name"],
                    "response": r["response"],
                    "correct_response": r["correct_response"],
                    "correct": r["correct"],
                    "rt_ms": round(r["rt_ms"], 2),
                })

    def run(self):
        """Main experiment flow."""
        # 1. Instructions
        if not self.instructions_screen():
            pygame.quit()
            return

        # 3. Practice trials
        practice_trials = generate_practice_trials()
        for i, trial in enumerate(practice_trials):
            result = self.run_trial(trial, i + 1, PRACTICE_TRIALS, is_practice=True)
            if result is None:
                pygame.quit()
                return
            # Don't record practice data in main results

        # 4. Transition to main experiment
        if not self.transition_screen(
            "Practice Complete",
            "The main experiment will now begin (32 trials)."
        ):
            pygame.quit()
            return

        # 5. Main experiment
        trials = generate_trials()
        for i, trial in enumerate(trials):
            result = self.run_trial(trial, i + 1, TOTAL_TRIALS)
            if result is None:
                pygame.quit()
                return
            response, rt_ms, correct = result
            self.results.append({
                "trial_num": i + 1,
                "phase": "main",
                "angle": trial["angle"],
                "trial_type": trial["trial_type"],
                "ref_obj": trial["ref_obj"],
                "comp_obj": trial["comp_obj"],
                "object_name": trial["object_name"],
                "response": response,
                "correct_response": trial["correct_response"],
                "correct": correct,
                "rt_ms": rt_ms,
            })
        self.save_results()
        self.results_screen()

        pygame.quit()


if __name__ == "__main__":
    exp = Experiment()
    exp.run()
