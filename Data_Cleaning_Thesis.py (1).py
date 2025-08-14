import pandas as pd
import re
import os
from pathlib import Path
import glob, nltk
from datetime import datetime
import json

class BatchQTACleaner:
    def __init__(self, output_dir="cleaned_data"):
        """Initialize batch cleaner with output directory"""
        self.output_dir = output_dir
        os.makedirs(output_dir, exist_ok=True)
        
        # Patterns for ProQuest cleaning
        self.proquest_patterns = [
            r'_{10,}.*?_{10,}',
            r'Contact us at:.*?Database copyright.*?Terms and Conditions:.*?https://[^\s]+',
            r'https://[^\s]+',
            r'ProQuest document ID:.*?\n',
            r'Document URL:.*?\n',
            r'Database:.*?\n',
            r'Last updated:.*?\n',
            r'Copyright:.*?\n',
            r'Links:.*?\n'
        ]
        
        # AI-related keywords for your thesis
        self.ai_keywords = [
            'artificial intelligence', 'AI', 'machine learning', 'deep learning',
            'neural network', 'algorithm', 'automation', 'robot', 'chatbot',
            'GPT', 'ChatGPT', 'automated', 'intelligent system', 'smart technology'
        ]
        
        # Initialize processing statistics
        self.stats = {
            'total_files': 0,
            'processed_files': 0,
            'errors': [],
            'usa_articles': 0,
            'ireland_articles': 0,
            'ai_mentions_total': 0
        }
    
    def extract_article_content(self, raw_text):
        """Extract main article text from ProQuest format"""
        # Remove ProQuest wrapper
        sections = raw_text.split('____________________________________________________________')
        
        if len(sections) >= 2:
            content = sections[1]
        else:
            content = raw_text
        
        # Find "Full text:" section
        if "Full text:" in content:
            full_text_start = content.find("Full text:")
            content = content[full_text_start + len("Full text:"):]
        
        # Remove metadata sections
        metadata_markers = [
            'CREDIT:', 'CAPTION:', 'Subject:', 'Business indexing term:',
            'Location:', 'People:', 'Company / organization:', 'Title:',
            'Publication title:', 'First page:', 'Publication year:'
        ]
        
        for marker in metadata_markers:
            if marker in content:
                content = content.split(marker)[0]
        
        # Clean ProQuest patterns
        for pattern in self.proquest_patterns:
            content = re.sub(pattern, '', content, flags=re.MULTILINE | re.DOTALL)
        
        # Basic text cleaning
        content = re.sub(r'\n+', ' ', content)
        content = re.sub(r'\s+', ' ', content)
        
        return content.strip()
    
    def extract_metadata(self, raw_text, filename):
        """Extract metadata from article"""
        metadata = {'filename': filename}
        
        # Common patterns
        patterns = {
            'title': r'Title:\s*([^\n]+)',
            'author': r'Author:\s*([^\n]+)',
            'publication': r'Publication title:\s*([^\n]+)',
            'date': r'Publication date:\s*([^\n]+)',
            'country': r'Country of publication:\s*([^\n]+)',
            'section': r'Section:\s*([^\n]+)'
        }
        
        for key, pattern in patterns.items():
            match = re.search(pattern, raw_text, re.IGNORECASE)
            metadata[key] = match.group(1).strip() if match else 'Unknown'
        
        # Enhanced country detection based on your dataset structure
        # First, try to detect from filename patterns
        filename_lower = filename.lower()
        
        # Check filename patterns first (most reliable for your data)
        if any(prefix in filename_lower for prefix in ['ii ', 'it ']):  # Irish Independent, Irish Times
            metadata['country_clean'] = 'Ireland'
            self.stats['ireland_articles'] += 1
        elif any(prefix in filename_lower for prefix in ['nyt ', 'wsj ']):  # New York Times, Wall Street Journal
            metadata['country_clean'] = 'USA'
            self.stats['usa_articles'] += 1
        else:
            # Fallback to publication title detection
            publication = metadata.get('publication', '').lower()
            if 'irish' in publication or 'dublin' in publication:
                metadata['country_clean'] = 'Ireland'
                self.stats['ireland_articles'] += 1
            elif any(pub in publication for pub in ['new york times', 'wall street journal']):
                metadata['country_clean'] = 'USA'
                self.stats['usa_articles'] += 1
            else:
                # Final fallback to country field
                country_text = metadata.get('country', '').lower()
                if 'united states' in country_text or 'usa' in country_text:
                    metadata['country_clean'] = 'USA'
                    self.stats['usa_articles'] += 1
                elif 'ireland' in country_text or 'irish' in country_text:
                    metadata['country_clean'] = 'Ireland'
                    self.stats['ireland_articles'] += 1
                else:
                    metadata['country_clean'] = 'Other'
        
        return metadata
    
    def count_ai_mentions(self, text):
        """Count AI-related mentions for your thesis analysis"""
        ai_count = 0
        ai_sentences = []
        
        sentences = text.split('.')
        
        for sentence in sentences:
            sentence_clean = sentence.strip().lower()
            for keyword in self.ai_keywords:
                if keyword.lower() in sentence_clean:
                    ai_count += 1
                    ai_sentences.append(sentence.strip())
                    break
        
        return ai_count, ai_sentences
    
    def process_single_file(self, file_path):
        """Process one text file"""
        try:
            # Read file with error handling for encoding
            encodings = ['utf-8', 'latin-1', 'cp1252']
            raw_text = None
            
            for encoding in encodings:
                try:
                    with open(file_path, 'r', encoding=encoding) as f:
                        raw_text = f.read()
                    break
                except UnicodeDecodeError:
                    continue
            
            if raw_text is None:
                raise Exception("Could not decode file with any encoding")
            
            # Extract metadata and content
            filename = os.path.basename(file_path)
            metadata = self.extract_metadata(raw_text, filename)
            article_text = self.extract_article_content(raw_text)
            
            # Count AI mentions for your thesis
            ai_count, ai_sentences = self.count_ai_mentions(article_text)
            
            # Update statistics
            self.stats['ai_mentions_total'] += ai_count
            
            return {
                **metadata,
                'article_text': article_text,
                'word_count': len(article_text.split()),
                'ai_mentions': ai_count,
                'ai_sentences': ' | '.join(ai_sentences[:3]),  # First 3 AI sentences
                'file_path': str(file_path)
            }
            
        except Exception as e:
            error_msg = f"Error processing {file_path}: {str(e)}"
            self.stats['errors'].append(error_msg)
            print(f"⚠️  {error_msg}")
            return None
    
    def process_all_files(self, input_directory):
        """Process all text files in directory"""
        print(f"🚀 Starting batch processing from: {input_directory}")
        print(f"📁 Output will be saved to: {self.output_dir}")
        
        # Find all text files
        txt_files = list(Path(input_directory).glob("*.txt"))
        self.stats['total_files'] = len(txt_files)
        
        if not txt_files:
            print("❌ No .txt files found in the directory!")
            return None
        
        print(f"📄 Found {len(txt_files)} text files to process")
        
        # Process files
        results = []
        for i, file_path in enumerate(txt_files, 1):
            print(f"Processing {i}/{len(txt_files)}: {file_path.name}")
            
            result = self.process_single_file(file_path)
            if result:
                results.append(result)
                self.stats['processed_files'] += 1
        
        # Create DataFrame
        if results:
            df = pd.DataFrame(results)
            return df
        else:
            print("❌ No files were successfully processed!")
            return None
    
    def save_results(self, df):
        """Save processed data in multiple formats"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # 1. Complete dataset
        complete_file = os.path.join(self.output_dir, f"complete_dataset_{timestamp}.xlsx")
        df.to_excel(complete_file, index=False)
        print(f"💾 Complete dataset saved: {complete_file}")
        
        # 2. USA vs Ireland comparison
        comparison_df = df[df['country_clean'].isin(['USA', 'Ireland'])].copy()
        if not comparison_df.empty:
            comparison_file = os.path.join(self.output_dir, f"usa_ireland_comparison_{timestamp}.xlsx")
            comparison_df.to_excel(comparison_file, index=False)
            print(f"💾 USA-Ireland comparison saved: {comparison_file}")
        
        # 3. QTA-ready text file
        qta_texts = []
        for _, row in df.iterrows():
            if len(row['article_text']) > 100:  # Filter very short articles
                qta_texts.append({
                    'text': row['article_text'],
                    'country': row['country_clean'],
                    'publication': row['publication'],
                    'date': row['date'],
                    'ai_mentions': row['ai_mentions']
                })
        
        if qta_texts:
            qta_df = pd.DataFrame(qta_texts)
            qta_file = os.path.join(self.output_dir, f"qta_ready_{timestamp}.csv")
            qta_df.to_csv(qta_file, index=False)
            print(f"💾 QTA-ready dataset saved: {qta_file}")
        
        # 4. AI-focused dataset for your thesis
        ai_focused = df[df['ai_mentions'] > 0].copy()
        if not ai_focused.empty:
            ai_file = os.path.join(self.output_dir, f"ai_focused_articles_{timestamp}.xlsx")
            ai_focused.to_excel(ai_file, index=False)
            print(f"💾 AI-focused articles saved: {ai_file}")
        
        # 5. Processing statistics
        stats_file = os.path.join(self.output_dir, f"processing_stats_{timestamp}.json")
        with open(stats_file, 'w') as f:
            json.dump(self.stats, f, indent=2)
        
        return complete_file, comparison_file, qta_file, ai_file
    
    def print_summary(self):
        """Print processing summary"""
        print("\n" + "="*50)
        print("📊 PROCESSING SUMMARY")
        print("="*50)
        print(f"Total files found: {self.stats['total_files']}")
        print(f"Successfully processed: {self.stats['processed_files']}")
        print(f"Errors: {len(self.stats['errors'])}")
        print(f"USA articles: {self.stats['usa_articles']}")
        print(f"Ireland articles: {self.stats['ireland_articles']}")
        print(f"Other articles: {self.stats['processed_files'] - self.stats['usa_articles'] - self.stats['ireland_articles']}")
        print(f"Total AI mentions: {self.stats['ai_mentions_total']}")
        
        if self.stats['errors']:
            print(f"\n⚠️  Errors encountered:")
            for error in self.stats['errors'][:5]:  # Show first 5 errors
                print(f"   - {error}")
            if len(self.stats['errors']) > 5:
                print(f"   ... and {len(self.stats['errors']) - 5} more errors")

def verify_country_classification(df):
    """Verify country classification results"""
    print("\n🔍 COUNTRY CLASSIFICATION VERIFICATION")
    print("-" * 50)
    
    # Group by country
    country_counts = df['country_clean'].value_counts()
    print("Country distribution:")
    for country, count in country_counts.items():
        print(f"  {country}: {count} articles")
    
    # Show sample filenames for each country
    print(f"\nSample filenames by country:")
    for country in ['USA', 'Ireland', 'Other']:
        if country in df['country_clean'].values:
            samples = df[df['country_clean'] == country]['filename'].head(3).tolist()
            print(f"  {country}: {', '.join(samples)}")
    
    # Check for misclassifications
    ireland_files = df[df['country_clean'] == 'Ireland']['filename'].tolist()
    usa_files = df[df['country_clean'] == 'USA']['filename'].tolist()
    other_files = df[df['country_clean'] == 'Other']['filename'].tolist()
    
    print(f"\n📝 Expected vs Actual:")
    print(f"Expected Ireland: 52 (II + IT files)")
    print(f"Actual Ireland: {len(ireland_files)}")
    print(f"Expected USA: 51 (NYT + WSJ files)")
    print(f"Actual USA: {len(usa_files)}")
    
    if other_files:
        print(f"\n⚠️  Files classified as 'Other' (check these):")
        for file in other_files[:10]:  # Show first 10
            print(f"    - {file}")
    
    return country_counts

# Main execution function
def main():
    """Main function to run from VS Code"""
    
    # STEP 1: Set your input directory path - Updated for your specific setup
    INPUT_DIRECTORY = r"C:\Users\malla\Downloads\Thesis Data"  # Your folder with all .txt files
    
    # STEP 2: Initialize cleaner - Output will be in the same folder
    OUTPUT_DIRECTORY = os.path.join(INPUT_DIRECTORY, "cleaned_thesis_data")
    cleaner = BatchQTACleaner(output_dir=OUTPUT_DIRECTORY)
    
    # STEP 3: Process all files
    df = cleaner.process_all_files(INPUT_DIRECTORY)
    
    if df is not None:
        # STEP 4: Save results
        files_created = cleaner.save_results(df)
        
        # STEP 5: Print summary and verify results
        cleaner.print_summary()
        
        # STEP 6: Verify country classification with detailed breakdown
        print("\n🔍 DETAILED COUNTRY CLASSIFICATION CHECK")
        print("-" * 60)
        
        # Check filename patterns
        ireland_patterns = ['ii ', 'it ']  # Irish Independent, Irish Times
        usa_patterns = ['nyt ', 'wsj ']    # New York Times, Wall Street Journal
        
        ireland_actual = 0
        usa_actual = 0
        misclassified = []
        
        for _, row in df.iterrows():
            filename_lower = row['filename'].lower()
            actual_country = row['country_clean']
            
            # Determine expected country based on filename
            if any(pattern in filename_lower for pattern in ireland_patterns):
                expected_country = 'Ireland'
                if actual_country != 'Ireland':
                    misclassified.append(f"MISMATCH: {row['filename']} → Expected: Ireland, Got: {actual_country}")
                else:
                    ireland_actual += 1
            elif any(pattern in filename_lower for pattern in usa_patterns):
                expected_country = 'USA'
                if actual_country != 'USA':
                    misclassified.append(f"MISMATCH: {row['filename']} → Expected: USA, Got: {actual_country}")
                else:
                    usa_actual += 1
            else:
                misclassified.append(f"UNKNOWN PATTERN: {row['filename']} → Got: {actual_country}")
        
        print(f"✅ Correctly classified Ireland articles: {ireland_actual}")
        print(f"✅ Correctly classified USA articles: {usa_actual}")
        
        if misclassified:
            print(f"\n⚠️  CLASSIFICATION ISSUES ({len(misclassified)}):")
            for issue in misclassified:
                print(f"    {issue}")
        
        print(f"\n📊 FINAL COUNTS:")
        print(f"Ireland: {df[df['country_clean'] == 'Ireland'].shape[0]} articles")
        print(f"USA: {df[df['country_clean'] == 'USA'].shape[0]} articles")
        print(f"Other: {df[df['country_clean'] == 'Other'].shape[0]} articles")
        
        print(f"\n✅ Processing complete! Check the 'cleaned_thesis_data' folder for outputs.")
        
        # Quick preview of results
        print(f"\n📋 Quick preview of first 3 articles:")
        for i in range(min(3, len(df))):
            row = df.iloc[i]
            print(f"{i+1}. {row['title'][:60]}... ({row['country_clean']}) - {row['ai_mentions']} AI mentions")
    
    else:
        print("❌ No data was processed. Please check your input directory path.")

if __name__ == "__main__":
    main()

# Additional utility functions you might need

def quick_analysis(csv_file_path):
    """Quick analysis of your cleaned data"""
    df = pd.read_csv(csv_file_path)
    
    print("📊 QUICK ANALYSIS")
    print("-" * 30)
    print(f"Total articles: {len(df)}")
    print(f"Countries: {df['country'].value_counts().to_dict()}")
    print(f"Average AI mentions per article: {df['ai_mentions'].mean():.2f}")
    print(f"Articles with AI mentions: {len(df[df['ai_mentions'] > 0])}")
    
    return df

def create_word_cloud_data(df, country=None):
    """Prepare data for word cloud visualization"""
    if country:
        text_data = df[df['country'] == country]['text'].str.cat(sep=' ')
    else:
        text_data = df['text'].str.cat(sep=' ')
    
    # Basic cleaning for word cloud
    text_data = re.sub(r'[^\w\s]', '', text_data.lower())
    words = text_data.split()
    
    # Remove common stopwords
    stopwords += nltk.corpus.stopwords.words('english')
    words = [word for word in words if word not in stopwords and len(word) > 3]
    
    return ' '.join(words)