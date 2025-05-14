<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocsCheckRulesDocs` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRdocsCheckRulesDocs` code unit defines a frame (`TFRAMEdocsCheckRulesDocs`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid of documents related to specific rules (`ruleId`) in a checklist system. It provides functionalities to add, delete, and manage documents associated with a rule, ensuring that only valid and non-duplicate documents are added.

### Technologies Used:
- **Delphi VCL Framework**: For UI components and event handling.
- **SOAP Services**: For interacting with external services (e.g., `CheckListDocsToAddServiceUtils`).
- **Database Components**: For managing and displaying data in a grid (`cxGridDBTableView`).
- **Custom Libraries**: Includes custom utilities like `kneFGFindUtils`, `kneFGDBUtils`, and `kneDialogFactory`.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **docCd**: Document Code (Custom Editor: `cxEDTfind`).
2. **docDesc**: Document Description (String).

#### Grid Actions and Their Effects:
1. **ADD**: Adds a new document to the grid.
2. **DELETE**: Deletes a selected document from the grid.

---

## 2. Functionality Description:

### User/Software Actions:
- Add a document to the grid.
- Delete a document from the grid.
- Prevent duplicate documents from being added.
- Display and manage documents in a grid format.

### Main Components:
1. **Grid (`cxGridDBTableView`)**: Displays the list of documents.
2. **Action Panel**: Provides buttons for adding and deleting documents.
3. **Custom Field Editor (`cxEDTfind`)**: Allows users to search for documents by code.

### Pseudo-Code for Actions and Events:
- **Add Document (`ACTaddExecute`)**:
  ```pseudo
  if add button clicked then
    execute add document logic
  ```

- **Find Document (`m_FindDocs`)**:
  ```pseudo
  if find button clicked then
    open document search dialog
    retrieve selected document
  ```

- **Prevent Duplicate Documents (`m_AlreadyAdded`)**:
  ```pseudo
  if document already exists in the grid then
    prevent addition
  ```

- **Process Each Row (`m_ProcessEachRow`)**:
  ```pseudo
  for each row in the grid do
    process row data
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized via the `Create` constructor.
   - Grid settings are configured in the `GridSetup` method.
   - Action panel and available actions (`ADD`, `DELETE`) are set up.

2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` method to add a document.
   - Clicking the "Find" button triggers the `m_FindDocs` method to search for a document.

3. **Functions and File Locations**:
   - `Create`: Initializes the frame (`FRdocsCheckRulesDocs.pas`).
   - `GridSetup`: Configures the grid (`FRdocsCheckRulesDocs.pas`).
   - `ACTaddExecute`: Handles the "Add" action (`FRdocsCheckRulesDocs.pas`).
   - `m_FindDocs`: Handles the "Find" action (`FRdocsCheckRulesDocs.pas`).

### Required Data:
- **ruleId**: The ID of the rule to which documents are associated.
- **Document Code (docCd)**: The unique code of the document.
- **Document Description (docDesc)**: A description of the document.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Document**:
   - Preconditions: The document must not already exist in the grid.
   - Action: Adds the document to the grid.

2. **Delete Document**:
   - Preconditions: A document must be selected in the grid.
   - Action: Deletes the selected document.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Document already added" if a duplicate document is detected.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **docCd**: Custom editor (`cxEDTfind`) for searching and selecting documents.
- **docDesc**: No specific validation is defined.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and sets up grid settings, action panel, and available actions.

2. **`GridSetup`**:
   - Configures the grid, including hidden fields, order fields, and custom editors.

3. **`ACTaddExecute`**:
   - Handles the logic for adding a document to the grid.

4. **`m_FindDocs`**:
   - Opens a search dialog to find and select a document.

5. **`m_AlreadyAdded`**:
   - Checks if a document is already added to the grid.

6. **`m_ProcessEachRow`**:
   - Processes each row in the grid for specific logic.

---

## 6. API Service Consumption:

- **Service Name**: `CheckListDocsToAddServiceUtils`.
- **Purpose**: To interact with external services for document management.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **cxGrid**: For grid display and management.
2. **SOAPHTTPClient**: For SOAP service interactions.
3. **kneFGFindUtils**: Custom utility for finding documents.

### Custom Components:
1. **TFRAMEBaseGridEditSOA**: Base frame for grid editing.
2. **cxEDTfind**: Custom field editor for document search.

---

## 9. Fields and Validations Listing:

1. **docCd**:
   - Type: String.
   - Validation: Custom editor (`cxEDTfind`).
   - Constraints: Not explicitly defined in the code.

2. **docDesc**:
   - Type: String.
   - Validation: None.
   - Constraints: Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Configure Grid] --> [User Interactions]
    --> [Add Document] --> [Check for Duplicates] --> [Update Grid]
    --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame: Click Add
Frame --> Service: Check for Duplicates
Service --> Frame: Response
Frame --> Grid: Update
```

### Code Snippets:
```delphi
procedure TFRAMEdocsCheckRulesDocs.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a document
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- `mc_GRID_FIELDS`: Defines the fields displayed in the grid.
- `MasterKeyFields`: Specifies the key field (`ruleId`) for the frame.
- `AvailableActions`: Defines the actions available in the action panel.

---

## 12. Conclusion:

The `FRdocsCheckRulesDocs` code unit provides a robust framework for managing documents associated with rules in a checklist system. Its strengths include modularity, extensibility, and integration with external services. However, it lacks detailed error handling and field validation, which could be improved.

---

## 13. Short Summary:

The `FRdocsCheckRulesDocs` unit manages a grid of documents linked to rules, allowing users to add, delete, and search for documents. It integrates with external services and provides a customizable grid interface for efficient document management.#### **FRdocsCheckRulesDocs.pas**

```
unit FRdocsCheckRulesDocs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEdocsCheckRulesDocs = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
  private
    mv_AlreadyAdded : string;
    procedure m_FindDocs(Sender: TObject; AButtonIndex: Integer);
    procedure m_AlreadyAdded;
    procedure m_ProcessEachRow(pv_RowIndex: Integer;
      pv_RowInfo: TcxRowInfo);
    procedure GridSetup;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

  end;

var
  FRAMEdocsCheckRulesDocs: TFRAMEdocsCheckRulesDocs;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, kneFGDBUtils, Global
  , CheckListDocsToAddServiceUtils
  ;

const
  mc_GRID_FIELDS = 'docCd;docDesc'; // [12-11-2018] retirado o campo seqNum  

{$R *.dfm}

constructor TFRAMEdocsCheckRulesDocs.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'ruleId';
  DataPacketName := 'CheckListDocEvents';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'checkListDocEvents';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da grelha
  GridSetup;

  mv_AlreadyAdded := '';
end;


procedure TFRAMEdocsCheckRulesDocs.GridSetup;
begin

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS);

//    DefineReadOnlyFields('ALL_FIELDS_READONLY');

    // Custom Editors ..........................................................
    AddCustomField('docCd','cxEDTfind');

  end; //with

//  cxEDTfind.Properties.OnButtonClick := m_FindParams;

end;


procedure TFRAMEdocsCheckRulesDocs.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
	cxDBVtable.GetColumnByFieldName('docCd').Options.Editing := False;
end;


procedure TFRAMEdocsCheckRulesDocs.m_FindDocs(
  Sender: TObject; AButtonIndex: Integer);
```

#### **FRdocsCheckRulesDocs.dfm**

```
dfm file is not needed for this file```
<!-- tabs:end -->

