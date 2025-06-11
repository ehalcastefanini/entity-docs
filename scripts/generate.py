import os
from collections import defaultdict
import requests
import re
import sys
import os
from dotenv import load_dotenv

# Load .env file
load_dotenv()

PROJECT_DIRECTORY = os.path.abspath(os.getenv("PROJECT_DIRECTORY"))
SOURCE_DIRECTORY = os.path.abspath(os.getenv("SOURCE_DIRECTORY"))
PROMPT_FILE = os.path.abspath(os.getenv("PROMPT_FILE"))
DOCS_FOLDER = os.path.abspath(os.getenv("DOCS_FOLDER"))
TEMPLATE_ID = os.getenv("TEMPLATE_ID")
API_KEY = os.getenv("API_KEY")
LANGUAGE = os.getenv("LANGUAGE")


ALLOWED_EXTENSIONS = ('.pas')  # Add extensions of files you want to document

def tree():
    return defaultdict(tree)

def read_file_content(file_path, max_lines=10000):
    """
    Tries to read the content of a file using different encodings.
    """
    encodings = ['utf-8', 'latin-1', 'cp1252']
    for encoding in encodings:
        try:
            lines = []
            with open(file_path, 'r', encoding=encoding, errors='replace') as f:
                for _ in range(max_lines):
                    line = f.readline()
                    if not line:
                        break
                    lines.append(line)
            content = ''.join(lines)
            # If content is read, return it
            if content:
                return content
        except Exception as e:
            print(f"Error reading {file_path} with encoding {encoding}: {e}")
    return None


# File extensions to consider as Pascal/Delphi sources
SEARCH_EXTENSIONS = ['.pas', '.dpr', '.dfm', '.pp', '.inc']


def find_declarations_in_file(filepath, patterns):
    try:
        with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()
            for i, line in enumerate(lines):
                for (pattern, name) in patterns:
                    if pattern.search(line):
                        return name
    except Exception as e:
        print(f"Could not read {filepath}: {e}")
    return False

def search(response):
 
    patterns = [
    (re.compile(r'\btype\s+' + re.escape(name.strip()) + r'\b\s*=\s*class\b', re.IGNORECASE), name.strip())
    for name in response
] + [
    (re.compile(r'\b' + re.escape(name.strip()) + r'\b\s*=', re.IGNORECASE), name.strip())
    for name in response
]
    root_folder = PROJECT_DIRECTORY
    results = []
    for dirpath, dirnames, filenames in os.walk(root_folder):
        for filename in filenames:
            if any(filename.lower().endswith(ext) for ext in SEARCH_EXTENSIONS):
                filepath = os.path.join(dirpath, filename)
                matches = find_declarations_in_file(filepath, patterns)
                if matches:
                    print(f"\n--- Declarations found in: {filepath} ---")
                    results.append({
                        'file': filepath,
                        'matches': matches
                    })
    return results



def generate_sai_documentation(code, dfm_content, language):

    url = "https://sai-library.saiapplications.com"
    headers = {"X-Api-Key": API_KEY}
    data = {
        "inputs": {
            "codigo": code,
            "idioma": language,
            "form": dfm_content,
        }
    }
    response = requests.post(f"{url}/api/templates/{TEMPLATE_ID}/execute", json=data, headers=headers)
    if response.status_code == 200:
        return response.text
    else:
        print(f"Error generating documentation: {response.status_code} - {response.text}")
        return f"Error: {response.status_code} - {response.text}"

def request_prompt(data):
    url = "https://sai-library.saiapplications.com"
    headers = {"X-Api-Key": API_KEY}
    json_data = {
        **data,
        "model": "gpt-4.1-2025-04-14",
        "temperature": 0,
        "max_tokens": 32768,
        "seed": 0
        }
    response = requests.post(f"{url}/api/prompt/v1/chat/completions", json=json_data, headers=headers)
    if response.status_code == 200:
        return response.json()['choices'][0]['message']['content'], data
    else:
        print(f"Error generating documentation: {response.status_code} - {response.text}")
        return f"Error: {response.status_code} - {response.text}"


def extract_point_13(text):
    """
    Extracts the content of point #13 from the documentation using regex.
    Looks for patterns like "13.", "13 -", "13:", etc.
    """
    # Common patterns for point 13
    patterns = [
        r'13\.\s*(.*?)(?=\n\s*\d+\.|\n\s*\#|\n\s*$|$)',  # 13. Text
        r'13\s*[\-\:]\s*(.*?)(?=\n\s*\d+[\-\:]|\n\s*\#|\n\s*$|$)',  # 13 - Text or 13: Text
        r'\#\s*13[\.\:\-]\s*(.*?)(?=\n\s*\#\s*\d+|\n\s*$|$)',  # # 13. Text
        r'\#13[\.\:\-]\s*(.*?)(?=\n\s*\#\d+|\n\s*$|$)',  # #13. Text
    ]

    for pattern in patterns:
        match = re.search(pattern, text, re.DOTALL)
        if match:
            return match.group(1).strip()

    return None


def generate_docsify_llm(project_directory, docs_folder=DOCS_FOLDER):
    if not os.path.exists(docs_folder):
        os.makedirs(docs_folder)
    print("FOLDER: " + DOCS_FOLDER)
    # Create README.md
    prompt_content = read_file_content(PROMPT_FILE, max_lines=10000)
    readme_path = os.path.join(docs_folder, "README.md")
    with open(readme_path, "w", encoding="utf-8") as f:
        f.write("# Project Documentation (Generated with LLM)\n\n")
        f.write("This site contains automatically generated pages.\n")
        f.write("Use the sidebar to navigate.\n\n")

    print("README created")
    print(os.walk(project_directory))
    sidebar_lines_link = []

    index_path = os.path.join(docs_folder, "index.html")
    # Create index.html if it doesn't exist
    if not os.path.exists(index_path):
        # copy ../docs-src/index.html to docs/index.html
        src_index_path = os.path.join(os.path.dirname(__file__), "../docs-src/index.html")
        with open(src_index_path, "r", encoding="utf-8") as src_file:
            content = src_file.read()
        with open(index_path, "w", encoding="utf-8") as dest_file:
            dest_file.write(content)
    print("Index.html created")

    # Dictionary to store point 13 of each file
    point_13_by_file = {}
    rendered_files_count = 0
    global_dependencies = set()
    found_dependencies = set()
    try:
        for root, dirs, files in os.walk(project_directory):
            print("Current directory:", root)
            print("Subdirectories:", dirs)
            print("Files:", files)
            for file in files:
                if file.lower().endswith(ALLOWED_EXTENSIONS):
                    if os.path.isabs(file):
                        file_path = file
                    else:
                        file_path = os.path.join(root, file)

                    content = read_file_content(file_path)

                    if not content:
                        print(f"Could not read file: {file_path}")
                        continue
                    else:
                        print(f"Content read from {file_path}: {len(content)} characters")

                    dfm_file_path = file_path.replace(".pas", ".dfm")
                    dfm_content = read_file_content(dfm_file_path)
                    if not dfm_content:
                        dfm_content = "dfm file is not needed for this file"
                        print(f"Could not read DFM file: {dfm_file_path}")
                    else:
                        print(f"DFM Content read from {dfm_file_path}: {len(dfm_content)} characters")

                    initialPrompt = {
                        "messages": [
                            {
                                "role": "system",
                                "content": [
                                    {
                                        "type": "text",
                                        "text": "You are a software development specialist and an expert in generating documentation in Markdown format for the Docsify application. Your task is to analyze a provided code snippet and produce comprehensive documentation that explains its purpose, functionality, and usage."
                                    }
                                ]
                            },
                            {
                                "role": "system",
                                "content": [
                                    {
                                        "type": "text",
                                        "text": "Given the code you should look for all dependencies and external classes/components that are used.Respond with the list of classNames/component names used. The response should contain only names of classes/components/types that are external to current file and not from known frameworks or ui libraries.The response should be a list of names separated by comma so I can search for them in file system."
                                    }
                                ]
                            },
                            {
                                "role": "user",
                                "content": [
                                    {
                                        "type": "text",
                                        "text": f"pascal\n{content}\n dfm:\n{dfm_content}\n\n"
                                    }
                                ]
                            }
                        ]
                        }

                    dependencies, request = request_prompt(initialPrompt)
                    split_dependencies = dependencies.split(",")
                    # filter out empty strings and strip whitespace
                    split_dependencies = [dep.strip() for dep in split_dependencies if dep.strip()]
                    # filter out strings that are already in found_dependencies
                    split_dependencies = [dep for dep in split_dependencies if dep not in found_dependencies]
                    # add found dependencies to the set
                    found_dependencies.update(split_dependencies)
                    search_results = search(split_dependencies)
                    dependencies_content = ""
                    current_dependencies = set()
                    if search_results:
                        for result in search_results:
                            d_file = result['file']
                            matches = result['matches']
                            file_content = read_file_content(d_file)
                            current_dependencies.add((d_file, matches))
                            dependencies_content += f"### {d_file}\n\n"
                            dependencies_content += f"```\n{file_content}\n```\n\n"
                        request['messages'].append({
                                    "role": "user",
                                    "content": [
                                        {
                                            "type": "text",
                                            "text": f"Dependencies found:\n{dependencies_content}"
                                        }
                                    ]
                                })
                    
                    
                    request['messages'].append({
                        "role": "user",
                        "content": [
                            {
                            "type": "text",
                            "text": prompt_content 
                        }]})        
                    
                    

                    # Generate documentation using the SAI API
                    # doc_llm = generate_sai_documentation(content, dfm_content, LANGUAGE)
                    
                    doc_llm = request_prompt(request)[0]
                    for match in current_dependencies:
                        dep_file, matches = match
                        fileName = os.path.basename(dep_file).split('/')[-1]
                        if (dep_file not in global_dependencies):
                            global_dependencies.add(dep_file)
                            files.append(dep_file)
                        if matches:
                            doc_llm = doc_llm.replace(f"`{matches}`", f"[{matches}]({fileName}.md)")

                    # Extract point 13 from the documentation
                    point_13 = extract_point_13(doc_llm)
                    if point_13:
                        # Use full name with extension as key to avoid duplicates
                        point_13_by_file[file] = point_13
                        print(f"Point 13 extracted from {file}: {point_13[:50]}...")
                    else:
                        print(f"Point 13 not found for file {file}")


                    # Markdown file name - includes the original extension to avoid duplicates
                    filename_short = os.path.basename(file).split('/')[-1]
                    md_filename = filename_short + ".md"
                    md_filepath = os.path.join(docs_folder, md_filename)

                    with open(md_filepath, "w", encoding="utf-8") as md_file:
                        md_file.write(f"<!-- tabs:start -->\n\n")
                        md_file.write(f"#### **Documentation**\n\n")
                        md_file.write(doc_llm)
                        
                        md_file.write(f"#### **{filename_short}**\n\n")
                        md_file.write(f"```\n{content}```\n\n")

                        md_file.write(f"#### **{filename_short.replace('.pas', '.dfm')}**\n\n")
                        md_file.write(f"```\n{dfm_content}```\n")
                        
                        md_file.write(f"<!-- tabs:end -->\n\n")
                    
                    sidebar_lines_link.append(f"* [{filename_short}]({md_filename})\n")

                   
                   

    except KeyboardInterrupt:
        print("Interruption detected (Ctrl+C). Finalizing processing and generating _sidebar.md...")
    except Exception as e:
        print(f"Exception occurred: {e}. Finalizing processing and generating _sidebar.md...")
    finally:
        # Generate _sidebar.md with the partial structure (even if an error or interruption occurs)
        sidebar_lines = ["# Documentation", ""]

        # Add the link to the application summary in the sidebar
        sidebar_lines.append("* [Application Summary](application_summary.md)")
        sidebar_lines.append("")  # Blank line for separation

        # Add the rest of the documentation
        sidebar_path = os.path.join(docs_folder, "_sidebar.md")
        with open(sidebar_path, "w", encoding="utf-8") as f:
            f.write("\n".join(sidebar_lines))
            f.write("\n".join(sidebar_lines_link))

        # Save the point 13 dictionary to a JSON file
        import json
        point_13_path = os.path.join(docs_folder, "application_summary.json")
        with open(point_13_path, "w", encoding="utf-8") as f:
            json.dump(point_13_by_file, f, ensure_ascii=False, indent=2)

        print("Sidebar generated!")
        print(f"Docs folder: {docs_folder}")
        print(f"'application_summary.json' and 'application_summary.md' generated with the summary of each file")
        print("Run: docsify serve docs to view it.")

if __name__ == "__main__":
    generate_docsify_llm(SOURCE_DIRECTORY)