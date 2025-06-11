#!/usr/bin/env python3
"""
Video compiler for forest fire simulation
Combines frames into final MP4 video with transitions
"""

import subprocess
import shutil
from pathlib import Path
import numpy as np
from PIL import Image, ImageDraw, ImageFont
import argparse
import json

class VideoCompiler:
    def __init__(self, frame_rate=60, duration=15):
        self.frame_rate = frame_rate
        self.duration = duration
        self.total_frames = frame_rate * duration
        
    def check_dependencies(self):
        """Check if required tools are installed"""
        if not shutil.which('ffmpeg'):
            raise RuntimeError("ffmpeg not found. Please install ffmpeg.")
        print("✓ ffmpeg found")
        
    def create_title_frame(self, output_path, width=1920, height=1080):
        """Create opening title frame"""
        # Create black background
        img = Image.new('RGB', (width, height), color='black')
        draw = ImageDraw.Draw(img)
        
        # Try to use a nice font, fallback to default if not available
        try:
            title_font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 72)
            subtitle_font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", 36)
        except:
            title_font = ImageFont.load_default()
            subtitle_font = ImageFont.load_default()
        
        # Add title text
        title = "Forest Fire Simulation"
        subtitle = "Swiss Alps - Climate Impact Analysis"
        
        # Center text
        title_bbox = draw.textbbox((0, 0), title, font=title_font)
        title_width = title_bbox[2] - title_bbox[0]
        title_x = (width - title_width) // 2
        
        subtitle_bbox = draw.textbbox((0, 0), subtitle, font=subtitle_font)
        subtitle_width = subtitle_bbox[2] - subtitle_bbox[0]
        subtitle_x = (width - subtitle_width) // 2
        
        draw.text((title_x, height//2 - 100), title, fill='white', font=title_font)
        draw.text((subtitle_x, height//2), subtitle, fill='gray', font=subtitle_font)
        
        img.save(output_path)
        
    def create_transition_frames(self, frame1_path, frame2_path, output_dir, num_frames=30):
        """Create smooth transition between two frames"""
        frame1 = Image.open(frame1_path)
        frame2 = Image.open(frame2_path)
        
        for i in range(num_frames):
            alpha = i / (num_frames - 1)
            blended = Image.blend(frame1, frame2, alpha)
            blended.save(output_dir / f"transition_{i:03d}.png")
            
    def create_split_screen(self, frame_2d_path, frame_3d_path, output_path):
        """Create split screen showing both 2D and 3D views"""
        frame_2d = Image.open(frame_2d_path)
        frame_3d = Image.open(frame_3d_path)
        
        # Resize to half width each
        width, height = 1920, 1080
        frame_2d = frame_2d.resize((width // 2, height))
        frame_3d = frame_3d.resize((width // 2, height))
        
        # Create combined image
        combined = Image.new('RGB', (width, height))
        combined.paste(frame_2d, (0, 0))
        combined.paste(frame_3d, (width // 2, 0))
        
        # Add divider line
        draw = ImageDraw.Draw(combined)
        draw.line([(width // 2, 0), (width // 2, height)], fill='white', width=2)
        
        combined.save(output_path)
        
    def compile_video(self, frames_dir, output_path, video_structure):
        """Compile frames into final video according to structure"""
        frames_path = Path(frames_dir)
        temp_dir = frames_path / "temp_video"
        temp_dir.mkdir(exist_ok=True)
        
        frame_counter = 0
        
        # Process each segment of the video
        for segment in video_structure:
            segment_type = segment['type']
            duration_seconds = segment['duration']
            segment_frames = int(duration_seconds * self.frame_rate)
            
            if segment_type == 'title':
                # Create title frame
                title_path = temp_dir / "title.png"
                self.create_title_frame(title_path)
                
                # Duplicate for duration
                for i in range(segment_frames):
                    shutil.copy(title_path, temp_dir / f"frame_{frame_counter:06d}.png")
                    frame_counter += 1
                    
            elif segment_type == '2d_view':
                # Use 2D frames
                source_frames = sorted(frames_path.glob("2d_frames/frame_*.png"))
                self._copy_frames_with_interpolation(source_frames, temp_dir, 
                                                   frame_counter, segment_frames)
                frame_counter += segment_frames
                
            elif segment_type == '3d_view':
                # Use 3D frames
                source_frames = sorted(frames_path.glob("3d_frames/3d_frame_*.png"))
                self._copy_frames_with_interpolation(source_frames, temp_dir,
                                                   frame_counter, segment_frames)
                frame_counter += segment_frames
                
            elif segment_type == 'split_screen':
                # Create split screen frames
                source_2d = sorted(frames_path.glob("2d_frames/frame_*.png"))
                source_3d = sorted(frames_path.glob("3d_frames/3d_frame_*.png"))
                
                for i in range(segment_frames):
                    src_idx = min(int(i * len(source_2d) / segment_frames), len(source_2d) - 1)
                    
                    if src_idx < len(source_2d) and src_idx < len(source_3d):
                        split_path = temp_dir / f"frame_{frame_counter:06d}.png"
                        self.create_split_screen(source_2d[src_idx], source_3d[src_idx], split_path)
                    frame_counter += 1
                    
            elif segment_type == 'fade_out':
                # Fade to black
                last_frame_path = temp_dir / f"frame_{frame_counter-1:06d}.png"
                if last_frame_path.exists():
                    last_frame = Image.open(last_frame_path)
                    black_frame = Image.new('RGB', last_frame.size, color='black')
                    
                    for i in range(segment_frames):
                        alpha = i / (segment_frames - 1)
                        blended = Image.blend(last_frame, black_frame, alpha)
                        blended.save(temp_dir / f"frame_{frame_counter:06d}.png")
                        frame_counter += 1
        
        # Use ffmpeg to create video
        print(f"Compiling {frame_counter} frames into video...")
        
        ffmpeg_cmd = [
            'ffmpeg',
            '-y',  # Overwrite output
            '-framerate', str(self.frame_rate),
            '-i', str(temp_dir / 'frame_%06d.png'),
            '-c:v', 'libx264',
            '-preset', 'slow',  # Better compression
            '-crf', '18',  # High quality
            '-pix_fmt', 'yuv420p',  # Compatibility
            '-movflags', '+faststart',  # Web optimization
            str(output_path)
        ]
        
        subprocess.run(ffmpeg_cmd, check=True)
        
        # Clean up temporary files
        shutil.rmtree(temp_dir)
        
        print(f"✓ Video saved to {output_path}")
        
        # Verify video properties
        self._verify_video(output_path)
        
    def _copy_frames_with_interpolation(self, source_frames, dest_dir, 
                                       start_frame, target_frames):
        """Copy frames with temporal interpolation if needed"""
        if not source_frames:
            return
            
        # Calculate frame mapping
        for i in range(target_frames):
            # Map target frame to source frame
            source_idx = min(int(i * len(source_frames) / target_frames), 
                           len(source_frames) - 1)
            
            # Copy frame
            shutil.copy(source_frames[source_idx], 
                       dest_dir / f"frame_{start_frame + i:06d}.png")
            
    def _verify_video(self, video_path):
        """Verify video properties"""
        probe_cmd = [
            'ffprobe',
            '-v', 'error',
            '-select_streams', 'v:0',
            '-show_entries', 'stream=width,height,r_frame_rate,duration',
            '-of', 'json',
            str(video_path)
        ]
        
        result = subprocess.run(probe_cmd, capture_output=True, text=True)
        if result.returncode == 0:
            info = json.loads(result.stdout)
            stream = info['streams'][0]
            
            width = stream.get('width', 'unknown')
            height = stream.get('height', 'unknown')
            fps = eval(stream.get('r_frame_rate', '0/1'))  # Handle fraction format
            
            print(f"\nVideo properties:")
            print(f"  Resolution: {width}x{height}")
            print(f"  Frame rate: {fps} fps")
            print(f"  Duration: ~{self.duration} seconds")
            
            # Check if it meets requirements
            if width == 1920 and height == 1080 and fps == 60:
                print("✓ Video meets 1080p60 requirements!")
            else:
                print("⚠ Warning: Video does not meet 1080p60 requirements")

def create_demo_video_structure():
    """Define the structure of the 15-second demo video"""
    return [
        {'type': 'title', 'duration': 2.0},           # 0-2s: Title
        {'type': '2d_view', 'duration': 3.0},         # 2-5s: 2D grid view
        {'type': '3d_view', 'duration': 4.0},         # 5-9s: 3D terrain view
        {'type': 'split_screen', 'duration': 4.0},    # 9-13s: Split screen
        {'type': 'fade_out', 'duration': 2.0},        # 13-15s: Fade out
    ]

def main():
    parser = argparse.ArgumentParser(description='Compile frames into demo video')
    parser.add_argument('frames_dir', help='Directory containing frame subdirectories')
    parser.add_argument('output_video', help='Output video file (MP4)')
    parser.add_argument('--fps', type=int, default=60, help='Frame rate (default: 60)')
    parser.add_argument('--duration', type=int, default=15, help='Duration in seconds (default: 15)')
    
    args = parser.parse_args()
    
    compiler = VideoCompiler(frame_rate=args.fps, duration=args.duration)
    compiler.check_dependencies()
    
    video_structure = create_demo_video_structure()
    compiler.compile_video(args.frames_dir, args.output_video, video_structure)

if __name__ == '__main__':
    main()