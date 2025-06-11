"""
Video assembly using FFmpeg for final video creation.
"""
import subprocess
import os
from pathlib import Path
from typing import List, Optional, Dict, Tuple
import numpy as np
from PIL import Image
import json


class VideoConfig:
    """Video configuration settings."""
    
    def __init__(self,
                 resolution: Tuple[int, int] = (1920, 1080),
                 fps: int = 60,
                 codec: str = 'libx264',
                 crf: int = 18,
                 preset: str = 'slow',
                 pixel_format: str = 'yuv420p'):
        self.resolution = resolution
        self.fps = fps
        self.codec = codec
        self.crf = crf  # Constant Rate Factor (lower = better quality)
        self.preset = preset
        self.pixel_format = pixel_format


class VideoAssembler:
    """Assemble frames into final video using FFmpeg."""
    
    def __init__(self, output_dir: str = "video/output/final"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
    def save_frames_as_images(self,
                             frames: List[np.ndarray],
                             output_dir: str,
                             prefix: str = "frame") -> List[str]:
        """
        Save numpy array frames as image files.
        
        Args:
            frames: List of frames as numpy arrays
            output_dir: Directory to save frames
            prefix: Filename prefix
            
        Returns:
            List of saved filenames
        """
        frame_dir = Path(output_dir)
        frame_dir.mkdir(parents=True, exist_ok=True)
        
        filenames = []
        for i, frame in enumerate(frames):
            filename = frame_dir / f"{prefix}_{i:06d}.png"
            
            # Convert numpy array to PIL Image
            if frame.dtype != np.uint8:
                frame = (frame * 255).astype(np.uint8)
            
            image = Image.fromarray(frame)
            image.save(filename, 'PNG', compress_level=1)  # Fast compression
            filenames.append(str(filename))
            
            if i % 100 == 0:
                print(f"Saved frame {i}/{len(frames)}")
        
        return filenames
    
    def assemble_final_video(self,
                            frame_directory: str,
                            output_path: str,
                            video_config: VideoConfig,
                            frame_pattern: str = "frame_%06d.png") -> bool:
        """
        Use FFmpeg to create final video from frames.
        
        Args:
            frame_directory: Directory containing frame images
            output_path: Output video file path
            video_config: Video configuration
            frame_pattern: Pattern for frame filenames
            
        Returns:
            Success status
        """
        # Construct FFmpeg command
        input_pattern = str(Path(frame_directory) / frame_pattern)
        
        ffmpeg_cmd = [
            'ffmpeg',
            '-y',  # Overwrite output
            '-framerate', str(video_config.fps),
            '-i', input_pattern,
            '-c:v', video_config.codec,
            '-preset', video_config.preset,
            '-crf', str(video_config.crf),
            '-pix_fmt', video_config.pixel_format,
            '-movflags', '+faststart',  # Web optimization
            output_path
        ]
        
        try:
            print(f"Running FFmpeg: {' '.join(ffmpeg_cmd)}")
            result = subprocess.run(ffmpeg_cmd, capture_output=True, text=True)
            
            if result.returncode != 0:
                print(f"FFmpeg error: {result.stderr}")
                return False
            
            print(f"Video created successfully: {output_path}")
            return True
            
        except subprocess.CalledProcessError as e:
            print(f"FFmpeg failed: {e}")
            return False
        except FileNotFoundError:
            print("FFmpeg not found. Please install FFmpeg.")
            return False
    
    def create_video_from_frames(self,
                                frames: List[np.ndarray],
                                output_name: str,
                                video_config: Optional[VideoConfig] = None) -> str:
        """
        Complete pipeline to create video from numpy frames.
        
        Args:
            frames: List of frames as numpy arrays
            output_name: Output video filename (without extension)
            video_config: Video configuration
            
        Returns:
            Path to created video
        """
        if video_config is None:
            video_config = VideoConfig()
        
        # Save frames as images
        frame_dir = self.output_dir / f"{output_name}_frames"
        print(f"Saving {len(frames)} frames to {frame_dir}...")
        self.save_frames_as_images(frames, str(frame_dir))
        
        # Assemble video
        output_path = self.output_dir / f"{output_name}.mp4"
        success = self.assemble_final_video(
            str(frame_dir), str(output_path), video_config
        )
        
        if success:
            # Clean up frames if successful
            self._cleanup_frames(frame_dir)
            return str(output_path)
        else:
            return None
    
    def add_post_processing(self,
                           video_path: str,
                           output_path: Optional[str] = None) -> bool:
        """
        Apply post-processing effects to video.
        
        Args:
            video_path: Input video path
            output_path: Output path (if None, overwrites input)
            
        Returns:
            Success status
        """
        if output_path is None:
            output_path = video_path.replace('.mp4', '_processed.mp4')
        
        # FFmpeg filters for post-processing
        filters = [
            # Slight contrast boost
            "eq=contrast=1.1:brightness=0:saturation=1.1",
            # Subtle sharpening
            "unsharp=5:5:0.5:5:5:0.0",
            # Very slight vignette
            "vignette=PI/4"
        ]
        
        filter_string = ",".join(filters)
        
        ffmpeg_cmd = [
            'ffmpeg',
            '-y',
            '-i', video_path,
            '-vf', filter_string,
            '-c:v', 'libx264',
            '-preset', 'medium',
            '-crf', '18',
            output_path
        ]
        
        try:
            result = subprocess.run(ffmpeg_cmd, capture_output=True, text=True)
            return result.returncode == 0
        except:
            return False
    
    def create_video_variants(self,
                             video_path: str,
                             create_webm: bool = True,
                             create_gif: bool = True) -> Dict[str, str]:
        """
        Create different format variants of the video.
        
        Args:
            video_path: Source video path
            create_webm: Create WebM version
            create_gif: Create GIF preview
            
        Returns:
            Dictionary of format -> path
        """
        variants = {'mp4': video_path}
        base_path = video_path.replace('.mp4', '')
        
        if create_webm:
            webm_path = f"{base_path}.webm"
            webm_cmd = [
                'ffmpeg', '-y', '-i', video_path,
                '-c:v', 'libvpx-vp9',
                '-crf', '30',
                '-b:v', '0',
                webm_path
            ]
            
            try:
                subprocess.run(webm_cmd, capture_output=True)
                variants['webm'] = webm_path
            except:
                print("Failed to create WebM variant")
        
        if create_gif:
            gif_path = f"{base_path}_preview.gif"
            # Create smaller GIF preview (480p, 10fps)
            gif_cmd = [
                'ffmpeg', '-y', '-i', video_path,
                '-vf', 'fps=10,scale=480:-1:flags=lanczos',
                '-c:v', 'gif',
                gif_path
            ]
            
            try:
                subprocess.run(gif_cmd, capture_output=True)
                variants['gif'] = gif_path
            except:
                print("Failed to create GIF variant")
        
        return variants
    
    def _cleanup_frames(self, frame_dir: Path, keep_frames: bool = False):
        """Clean up temporary frame files."""
        if not keep_frames and frame_dir.exists():
            import shutil
            try:
                shutil.rmtree(frame_dir)
                print(f"Cleaned up frame directory: {frame_dir}")
            except:
                print(f"Warning: Could not clean up {frame_dir}")


class VideoQualityChecker:
    """Validate video quality and specifications."""
    
    def validate_video(self, video_path: str) -> Dict:
        """
        Ensure video meets specifications.
        
        Args:
            video_path: Path to video file
            
        Returns:
            Quality report dictionary
        """
        report = {
            'valid': False,
            'duration': 0,
            'resolution': (0, 0),
            'fps': 0,
            'size_mb': 0,
            'issues': []
        }
        
        if not os.path.exists(video_path):
            report['issues'].append('File not found')
            return report
        
        # Get file size
        report['size_mb'] = os.path.getsize(video_path) / (1024 * 1024)
        
        # Use ffprobe to get video info
        try:
            cmd = [
                'ffprobe',
                '-v', 'error',
                '-select_streams', 'v:0',
                '-show_entries', 'stream=width,height,r_frame_rate,duration',
                '-of', 'json',
                video_path
            ]
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode == 0:
                info = json.loads(result.stdout)
                stream = info['streams'][0]
                
                # Parse resolution
                report['resolution'] = (int(stream['width']), int(stream['height']))
                
                # Parse frame rate
                fps_parts = stream['r_frame_rate'].split('/')
                report['fps'] = int(fps_parts[0]) / int(fps_parts[1])
                
                # Parse duration
                if 'duration' in stream:
                    report['duration'] = float(stream['duration'])
            
            # Check specifications
            if report['resolution'] != (1920, 1080):
                report['issues'].append(f"Resolution {report['resolution']} != 1920x1080")
            
            if abs(report['fps'] - 60) > 1:
                report['issues'].append(f"FPS {report['fps']} != 60")
            
            if abs(report['duration'] - 15) > 0.1:
                report['issues'].append(f"Duration {report['duration']}s != 15s")
            
            if report['size_mb'] > 100:
                report['issues'].append(f"File size {report['size_mb']}MB > 100MB limit")
            
            report['valid'] = len(report['issues']) == 0
            
        except Exception as e:
            report['issues'].append(f"FFprobe error: {str(e)}")
        
        return report