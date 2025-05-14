<!-- tabs:start -->

#### **Documentation**

# Documentation for `EdocsCheckDefaults` Code Unit

## 1. Overview:

### Objective:
The `EdocsCheckDefaults` code unit is designed to manage and maintain document checks within a user interface. It provides a form-based interface for users to view, add, modify, and delete document records. The main objective is to streamline the process of managing document-related data while ensuring proper validation and interaction with backend services.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface and handling events.
- **Third-party Components**: Includes components like `TsPanel`, `TsPageControl`, and `TcxGrid` for enhanced UI design and functionality.
- **Backend Service Integration**: Utilizes `CheckListDocsDefaultServiceUtils` for data retrieval and manipulation.

### Form Type:
This code represents a **form** with the following elements:
- **Form Elements**:
  - `TsPanel`: Panels for organizing UI components.
  - `TsPageControl`: Tabbed interface for displaying additional information.
  - `TcxGrid`: Grid for displaying document records.
  - Buttons (`TsBitBtn`): For actions like adding and deleting records.
- **Form Actions**:
  - Add a new document.
  - Delete an existing document.
  - Print the current record.

---

## 2. Functionality Description:

### User/Software Actions:
- **Add Document**: Users can add a new document using the "Add" button.
- **Delete Document**: Users can delete a selected document if the mode allows it.
- **Print Record**: Users can print the currently selected record.

### Main Components:
- **`FRAMEdocsCheckDefaultsDocs1`**: Handles document-related actions like adding and deleting records.
- **`FRAMEdocsCheckDefaults`**: Displays and manages document check defaults.
- **`PGCdocuments`**: A tabbed interface for organizing additional information.

### Pseudo-code for Actions and Events:
- **OnClick event of "Add" button**:
  ```
  if Add button clicked then
    execute add document function
  ```
- **OnClick event of "Delete" button**:
  ```
  if Delete button clicked then
    if mode is not "VIEW" and grid is not empty then
      execute delete document function
  ```
- **OnClick event of "Print" button**:
  ```
  if Print button clicked then
    execute print current record function
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using the `m_CreateFormEdit` method.
   - UI components are initialized, and data is loaded using the `m_getData` method.
2. **User Interactions**:
   - Users interact with buttons to perform actions like adding, deleting, or printing records.
3. **Backend Integration**:
   - Data is fetched and updated using the `CheckListDocsDefaultServiceUtils` utility.

### Data Requirements:
- Users must provide document details when adding a new record.
- The system retrieves existing records from the backend for display and modification.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Document**:
  - Preconditions: None.
  - Action: Opens a form to input new document details.
- **Delete Document**:
  - Preconditions: A document must be selected, and the mode must not be "VIEW".
  - Action: Deletes the selected document.
- **Print Record**:
  - Preconditions: A record must be selected.
  - Action: Prints the selected record.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "No record selected" if attempting to delete or print without selecting a record.
- "Action not allowed in VIEW mode" if attempting to delete in "VIEW" mode.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- No explicit field validations are defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`m_CreateFormEdit`**:
   - Creates and initializes the form.
2. **`m_getData`**:
   - Loads data from the backend and sets up the UI components.
3. **`BTprintCurrentRecordClick`**:
   - Handles the print action for the current record.
4. **`FRAMEdocsCheckDefaultsDocs1BTNaddClick`**:
   - Handles the add document action.

---

## 6. API Service Consumption:

### Backend Service:
- **Service Name**: `CheckListDocsDefaultServiceUtils`
- **Purpose**: Fetch and update document data.
- **Data Sent**: Parameters like `consMkt` and `consignee`.
- **Data Received**: Document records.
- **Error Handling**: Ensures proper cleanup of resources and displays error messages if data retrieval fails.

---

## 7. Conditional Fields (Form Logic):

- **"Delete" Button**:
  - Enabled only if the mode is not "VIEW" and the grid is not empty.

---

## 8. Dependencies:

### External Libraries:
- **`kneUtils`**: Utility functions.
- **`Global`**: Global configurations and constants.
- **`CheckListDocsDefaultServiceUtils`**: Backend service utility.

### Custom Components:
- **`TFRAMEdocsCheckDefaultsDocs`**: Custom frame for managing document actions.
- **`TFRAMEdocsCheckDefaults`**: Custom frame for displaying document check defaults.

---

## 9. Fields and Validations Listing:

- **Document Code** (type: string, required): Represents the document identifier.
- **Consignee** (type: string, optional): Represents the consignee information.
- **Market** (type: string, required): Represents the market associated with the document.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a detailed workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide detailed interactions.)

### Code Snippets:
```delphi
procedure TFORMEdocsCheckDefaults.BTprintCurrentRecordClick(Sender: TObject);
begin
  // Logic to print the current record
end;
```

### Screenshots:
The DFM file represents a form with panels, a tabbed interface, and a grid. Below is the HTML representation:

```html
<div style="width: 780px; height: 627px; border: 1px solid #000;">
  <div style="height: 69px; background-color: #f0f0f0;">Documents Check Maintenance</div>
  <div style="height: 490px;">
    <div style="height: 460px; overflow: auto;">
      <table border="1" style="width: 100%;">
        <tr>
          <th>Document Code</th>
          <th>Consignee</th>
          <th>Market</th>
        </tr>
        <tr>
          <td>DOC001</td>
          <td>John Doe</td>
          <td>USA</td>
        </tr>
      </table>
    </div>
  </div>
</div>
```

---

## 11. Important Comments in the Code:

- **Resource Optimization**:
  ```delphi
  Screen.Cursor := crHourGlass;
  ```
  Ensures efficient resource usage during data loading.

- **Conditional Logic**:
  ```delphi
  if (stringAccessMode = 'NEW') then
    FRAMEdocsCheckDefaultsDocs1.BTNdelete.Enabled := False;
  ```

---

## 12. Conclusion:

The `EdocsCheckDefaults` code unit provides a robust interface for managing document checks. Its strengths lie in its modular design and integration with backend services. However, the lack of explicit field validations and error handling could be improved.

---

## 13. Short Summary:

The `EdocsCheckDefaults` unit manages document checks through a form-based interface, enabling users to add, delete, and print records. It integrates with backend services for data management and ensures efficient resource usage.#### **EdocsCheckDefaults.pas**

```
unit EdocsCheckDefaults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA,
  kneFRGridEditSOA, FRdocsCheck, kneFRCtrlEditSOA,
  DBClient, knePrivileges, ImgList, sSpeedButton, sBitBtn, ToolWin,
  ComCtrls, acCoolBar, sPanel, kneEnterAsTab, sPageControl, ActnList,
  sSplitter, FRdocsCheckDefaultsDocs, FRdocsCheckDefaults;

type
  TFORMEdocsCheckDefaults = class(TFORMkneBaseEdit)
    PNLaddInfo: TsPanel;
    PGCdocuments: TsPageControl;
    TSHaddInfo: TsTabSheet;
    FRAMEdocsCheckDefaultsDocs1: TFRAMEdocsCheckDefaultsDocs;
    FRAMEdocsCheckDefaults1: TFRAMEdocsCheckDefaults;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEdocsCheckDefaultsDocs1BTNaddClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure m_getData; override;
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMEdocsCheckDefaults: TFORMEdocsCheckDefaults;

implementation

{$R *.dfm}

uses
  kneUtils, Global,
  //---
  CheckListDocsDefaultServiceUtils;

{ TFORMEdocsCheckDefaults }

class function TFORMEdocsCheckDefaults.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMEdocsCheckDefaults.Create(Application);
end;

procedure TFORMEdocsCheckDefaults.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
  lv_Keys: TStringList;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMEdocsCheckDefaultsDocs1.MasterSource := lv_MasterFrame.DStable;
  FRAMEdocsCheckDefaultsDocs1.ProviderService := lv_MasterFrame.ProviderService;


  if mv_keyValues <> '' then
  begin
    try
      lv_Keys := nil;
      lv_Keys := TStringList.Create;
      TkneGeneric.SplitStrings(mv_keyValues, lv_Keys, ';', True);
      with TCheckListDocsDefaultServiceUtils(lv_MasterFrame.ProviderService).Params do
      begin
        consMkt :=  lv_Keys[0];
//        if (lv_Keys[1] <> 'ALL') then
          consignee :=  lv_Keys[1]
//        else
//          consignee :=  '';
      end;
    finally
        if Assigned(lv_Keys) then FreeAndNil(lv_Keys);
    end;
  end;

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
//  lv_MasterFrame.ServiceParams.MaxRecords := 0;
//  lv_MasterFrame.ServiceParams.Criteria := '';

  inherited m_getData;

  if (stringAccessMode = 'NEW') then
  begin
    FRAMEdocsCheckDefaultsDocs1.BTNdelete.Enabled := False;
  end else
  begin
    FRAMEdocsCheckDefaultsDocs1.BTNdelete.Enabled := (stringAccessMode <> 'VIEW')
      and (not FRAMEdocsCheckDefaultsDocs1.CDStable.IsEmpty);
      
    if (stringAccessMode = 'MODIFY') then
      FRAMEdocsCheckDefaultsDocs1.AddedDocs := '|' +                                    //obtem a lista de Docs adicionados
        GetFieldValuesFromCDS(FRAMEdocsCheckDefaultsDocs1.CDStable, 'docCd', '|') + '|';
```

#### **EdocsCheckDefaults.dfm**

```
inherited FORMEdocsCheckDefaults: TFORMEdocsCheckDefaults
  Left = 402
  Top = 125
  Width = 780
  Height = 627
  Caption = 'Documents Check Maintenance'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 772
    inherited CLBactions: TsCoolBar
      Width = 772
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 768
        end>
      inherited PNbotoes: TsPanel
        Width = 755
      end
    end
  end
  object PNLdocsCheckDefaults: TsPanel [2]
    Left = 0
    Top = 41
    Width = 772
    Height = 69
    Align = alTop
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEdocsCheckDefaults1: TFRAMEdocsCheckDefaults
      Left = 1
      Top = 1
      Width = 770
      Height = 67
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited PNLfooter: TsPanel
        Top = 33
        Width = 770
      end
    end
  end
  object PNLaddInfo: TsPanel [3]
    Left = 0
    Top = 110
    Width = 772
    Height = 490
    Align = alClient
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object PGCdocuments: TsPageControl
      Left = 1
      Top = 1
      Width = 770
      Height = 488
      ActivePage = TSHaddInfo
      Align = alClient
      TabOrder = 0
      SkinData.SkinSection = 'PAGECONTROL'
      object TSHaddInfo: TsTabSheet
        Caption = 'Documents'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocsCheckDefaultsDocs1: TFRAMEdocsCheckDefaultsDocs
          Left = 0
          Top = 0
          Width = 762
          Height = 460
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 762
            Height = 426
          end
          inherited PNLfooter: TsPanel
            Top = 426
            Width = 762
            inherited PNLeditActions: TsPanel
              inherited PNLaddAction: TsPanel
                inherited BTNadd: TsBitBtn
                  OnClick = FRAMEdocsCheckDefaultsDocs1BTNaddClick
                end
```
<!-- tabs:end -->

