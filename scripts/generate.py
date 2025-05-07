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
DOCS_FOLDER = os.path.abspath(os.getenv("DOCS_FOLDER"))
TEMPLATE_ID = os.getenv("TEMPLATE_ID")
API_KEY = os.getenv("API_KEY")


ALLOWED_EXTENSIONS = ('.pas')  # Add extensions of files you want to document

def tree():
    return defaultdict(tree)

def read_file_content(file_path, max_lines=100):
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

def generate_sai_documentation(code, dfm_content, language="english"):

    url = "https://sai-library.saiapplications.com"
    headers = {"X-Api-Key": API_KEY}
    data = {
        "inputs": {
            "code": code,
            "language": language,
            "form": dfm_content,
        }
    }
    if language == "spanish":
        response = requests.post(f"{url}/api/templates/680938afe05a0cd3549a5fa2/execute", json=data, headers=headers)
        print("API Response: " + response.text)
    elif language == "portuguese":
        response = requests.post(f"{url}/api/templates/6807f313e05a0cd35499ee44/execute", json=data, headers=headers)
    elif language == "english":
        response = requests.post(f"{url}/api/templates/{TEMPLATE_ID}/execute", json=data, headers=headers)  # Replace the ID with your template version
    if response.status_code == 200:
        return response.text
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

def build_sidebar_lines(node, level=0):
    lines = []
    for key in sorted(node.keys()):
        val = node[key]
        indent = "  " * level
        if isinstance(val, dict):
            # It's a folder
            lines.append(f"{indent}* **{key}/**")
            lines.extend(build_sidebar_lines(val, level + 1))
        else:
            # It's a file
            lines.append(f"{indent}* [{key}]({val})")
    return lines

def generate_docsify_llm(project_directory, docs_folder=DOCS_FOLDER):
    if not os.path.exists(docs_folder):
        os.makedirs(docs_folder)
    print("FOLDER: " + DOCS_FOLDER)
    # Create README.md
    readme_path = os.path.join(docs_folder, "README.md")
    with open(readme_path, "w", encoding="utf-8") as f:
        f.write("# Project Documentation (Generated with LLM)\n\n")
        f.write("This site contains automatically generated pages.\n")
        f.write("Use the sidebar to navigate.\n\n")

    structure = tree()
    print("README created")
    print(os.walk(project_directory))

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
    # copy ../docs-src/assets to docs/assets folder
    # src_assets_path = os.path.join(os.path.dirname(__file__), "../docs-src/assets")
    # dest_assets_path = os.path.join(docs_folder, "assets")
    # if not os.path.exists(dest_assets_path):
    #     os.makedirs(dest_assets_path)
    #     for item in os.listdir(src_assets_path):
    #         src_item_path = os.path.join(src_assets_path, item)
    #         dest_item_path = os.path.join(dest_assets_path, item)
    #         if os.path.isdir(src_item_path):
    #             os.makedirs(dest_item_path, exist_ok=True)
    #             for sub_item in os.listdir(src_item_path):
    #                 src_sub_item_path = os.path.join(src_item_path, sub_item)
    #                 dest_sub_item_path = os.path.join(dest_item_path, sub_item)
    #                 with open(src_sub_item_path, "rb") as src_file:
    #                     content = src_file.read()
    #                 with open(dest_sub_item_path, "wb") as dest_file:
    #                     dest_file.write(content)
    #         else:
    #             with open(src_item_path, "rb") as src_file:
    #                 content = src_file.read()
    #             with open(dest_item_path, "wb") as dest_file:
    #                 dest_file.write(content)
    # print("Assets copied")

    # Dictionary to store point 13 of each file
    point_13_by_file = {}
    rendered_files_count = 0
    try:
        for root, dirs, files in os.walk(project_directory):
            print("Current directory:", root)
            print("Subdirectories:", dirs)
            print("Files:", files)

            for file in files:
                if file.lower().endswith(ALLOWED_EXTENSIONS):
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
                        print(f"Could not read DFM file: {dfm_file_path}")
                        continue
                    else:
                        print(f"DFM Content read from {dfm_file_path}: {len(dfm_content)} characters")

                    # Generate documentation using the SAI API
                    doc_llm = generate_sai_documentation(content, dfm_content, language="english")

                    # Extract point 13 from the documentation
                    point_13 = extract_point_13(doc_llm)
                    if point_13:
                        # Use full name with extension as key to avoid duplicates
                        point_13_by_file[file] = point_13
                        print(f"Point 13 extracted from {file}: {point_13[:50]}...")
                    else:
                        print(f"Point 13 not found for file {file}")

                    # Relative path with respect to the base directory
                    rel_path = os.path.relpath(file_path, project_directory)
                    parts = rel_path.split(os.sep)
                    *folders, filename = parts

                    # Create subfolders in docs/
                    subfolder_docs = os.path.join(docs_folder, *folders)
                    os.makedirs(subfolder_docs, exist_ok=True)

                    # Markdown file name - includes the original extension to avoid duplicates
                    md_filename = file + ".md"
                    md_filepath = os.path.join(subfolder_docs, md_filename)

                    with open(md_filepath, "w", encoding="utf-8") as md_file:
                        md_file.write(f"<!-- tabs:start -->\n\n")
                        md_file.write(f"#### **Documentation**\n\n")
                        md_file.write(doc_llm)
                        
                        md_file.write(f"#### **{file}**\n\n")
                        md_file.write(f"```\n{content}```\n\n")

                        md_file.write(f"#### **{file.replace(".pas", ".dfm")}**\n\n")
                        md_file.write(f"```\n{dfm_content}```\n")
                        
                        md_file.write(f"<!-- tabs:end -->\n\n")

                    # Insert into the _sidebar structure
                    current = structure
                    for folder in folders:
                        current = current[folder]
                    md_relative_to_docs = os.path.join(*folders, md_filename)
                    current[filename] = md_relative_to_docs
                    rendered_files_count += 1
                    # halt after 10 files
                    if rendered_files_count >= 3:
                        print("10 files processed. Stopping for demonstration purposes.")
                        break

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
        sidebar_lines.extend(build_sidebar_lines(structure))
        sidebar_path = os.path.join(docs_folder, "_sidebar.md")
        with open(sidebar_path, "w", encoding="utf-8") as f:
            f.write("\n".join(sidebar_lines))

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
    generate_docsify_llm(PROJECT_DIRECTORY)