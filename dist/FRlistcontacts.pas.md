<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRlistContacts` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRlistContacts` code unit is designed to manage and display a grid of contact information. It provides functionality for viewing, adding, and managing contact details in a structured grid format. The main objective is to offer a user-friendly interface for handling contact data, including filtering, ordering, and customizing the display of contact information.

### Technologies Used:
- **Delphi Framework**: The code is written in Delphi, utilizing its object-oriented programming features.
- **VCL Components**: Components like `TcxGrid`, `TcxGridDBTableView`, and `TsPanel` are used for UI and data display.
- **SOAP Services**: The code interacts with SOAP-based services for data handling.
- **Database Components**: `TDataSet` and `TClientDataSet` are used for database operations.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **contactType**: String (Custom Editor: `cxEDTfind`).
2. **contactNameDummy**: String (Fictitious column for display purposes).
3. **contactDesc**: String.
4. **position**: String.

#### Grid Actions and Their Effects:
1. **Add Action**: Adds a new contact. Displays a warning if the `entityType` field is empty.
2. **Data Loading**: Automatically populates the grid with contact data when the dataset is opened.
3. **Custom Column Filling**: Fills a dummy column (`contactNameDummy`) with values from the real column (`contactName`).

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add Contact**: Triggered by the `ACTaddExecute` procedure.
2. **Load Data**: Automatically triggered when the dataset is opened (`CDStableAfterOpen`).
3. **Fill Dummy Column**: Populates a dummy column for display purposes (`m_FillContactName`).

### Main Components:
- **Grid (`TcxGrid`)**: Displays contact data.
- **Action Buttons (`TsBitBtn`)**: Includes buttons for adding, applying, and canceling actions.
- **Dataset (`TClientDataSet`)**: Manages the contact data.

### Pseudo-code for Actions and Events:
- **OnClick event of Add Button**:  
  `if add button clicked then execute ACTaddExecute procedure`.
- **OnAfterOpen event of Dataset**:  
  `if dataset opened then execute CDStableAfterOpen procedure`.
- **Custom Column Filling**:  
  `if dataset is filtered then iterate through records and populate dummy column`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: The `TFRAMElistContacts` frame is created, and grid settings are configured.
2. **Data Loading**: When the dataset is opened, the grid is populated with contact data.
3. **User Interaction**: Users can add new contacts or view existing ones. The `Add` button triggers the `ACTaddExecute` procedure.
4. **Custom Column Filling**: The `m_FillContactName` procedure is used to populate the dummy column.

### Required Data:
- **entityType**: Must be provided for adding a contact.
- **contactType, contactName, contactDesc, position**: Displayed in the grid.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Contact**:  
   - Preconditions: `entityType` must not be empty.  
   - Action: Displays a warning if `entityType` is empty.

### Available Filters:
- The dataset can be filtered programmatically, but no explicit user-facing filters are defined in the code.

### Error Messages:
- "entityType is empty": Displayed when attempting to add a contact without specifying the `entityType`.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **entityType**: Must not be empty when adding a contact.
- **contactType**: Custom editor (`cxEDTfind`) is used for selection.

---

## 5. Main Functions:

1. **`Create` Constructor**: Initializes the frame and configures grid settings.
2. **`ACTaddExecute`**: Handles the "Add" action and validates the `entityType` field.
3. **`CDStableAfterOpen`**: Executes actions after the dataset is opened.
4. **`m_FillContactName`**: Populates the dummy column with values from the real column.

---

## 6. API Service Consumption:

No explicit API calls are defined in the provided code snippet.

---

## 7. Conditional Fields (Form Logic):

- **Dummy Column (`contactNameDummy`)**:  
  - Condition: Populated only when the dataset is filtered and iterated.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components**: Used for UI and data handling.
- **SOAPHTTPClient**: For SOAP-based service interactions.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base class for the frame, providing grid editing functionality.

---

## 9. Fields and Validations Listing:

1. **contactType**: String, required, custom editor (`cxEDTfind`).
2. **contactNameDummy**: String, optional, populated programmatically.
3. **contactDesc**: String, optional.
4. **position**: String, optional.

Mapping of displayed values to database columns:
- **contactNameDummy** → `contactName`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Configure Grid Settings] --> [Open Dataset] --> [Populate Grid]
```

### Sequence Diagram:
```plaintext
User --> [Add Button Click] --> [ACTaddExecute] --> [Validate entityType] --> [Show Warning if Empty]
```

### Code Snippets:
```delphi
procedure TFRAMElistContacts.ACTaddExecute(Sender: TObject);
begin
  inherited;
  if cdstable.FieldByName('entityType').AsString = '' then
    MessageDlg('entityType is empty', mtWarning, [mbOK], 0);
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures read-only fields, hidden fields, field order, and key fields.
- **Custom Column Filling**: Populates a dummy column for display purposes.

---

## 12. Conclusion:

The `FRlistContacts` code unit provides a robust framework for managing and displaying contact data in a grid format. Its strengths include customizable grid settings and validation for adding contacts. However, it lacks user-facing filters and explicit API integration.

---

## 13. Short Summary:

The `FRlistContacts` unit manages a grid-based contact list with customizable settings and validation for adding contacts. It supports data display and interaction but lacks advanced filtering and API integration.#### **FRlistcontacts.pas**

```
unit FRlistContacts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMElistContacts = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableAfterOpen(DataSet: TDataSet);
  private
    procedure m_FillDummyColumn(const pv_ContName: string);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent);  override;
    { Public declarations }
    procedure m_FillContactName;
  end;

var
  FRAMElistContacts: TFRAMElistContacts;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes, kneConfigObjects, kneFREditSOA, Global;

constructor TFRAMElistContacts.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'entityCode;entityType;addrNum';
  DataPacketName := 'Contact';
  PropertyName := 'contacts';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('addrNum; entityCode; entityType; contactType; ' +
    'contactName; contactNameDummy; contactDesc; position'); // #14349

    // Campos Hidden ...........................................................
    DefineHiddenFields('addrNum;entityCode;entityType;contactName'); // #14349
    // Ordem Campos ............................................................
    DefineOrderFields('contactType;contactNameDummy;contactDesc;position'); // #14349
    // Key Fields ..............................................................
    KeyFields:= 'entityCode;entityType;addrNum;contactType;contactName'; // #14349
    // Custom Editors ..........................................................
    AddCustomField('contactType','cxEDTfind');
  end; //with
  ColsWidthInGrid := '100;200;200;70';
end;

procedure TFRAMElistContacts.ACTaddExecute(Sender: TObject);
begin
  inherited;
  if cdstable.FieldByName('entityType').AsString = '' then
    MessageDlg('entityType is empty', mtWarning, [mbOK], 0);
end;


procedure TFRAMElistContacts.CDStableAfterOpen(DataSet: TDataSet);
begin
  inherited;
end;

// #14349, percorrer os registos e preencher a coluna fict�cia com os valores da coluna real
procedure TFRAMElistContacts.m_FillContactName;
var
  lv_filter: string;
  lv_ContName : string;
begin
  CDStable.DisableControls;

	lv_filter := CDStable.Filter;

  CDStable.Filtered := True;
  CDStable.Filter := '';
  CDStable.Filtered := True;

  CDStable.First;
  try
```

#### **FRlistcontacts.dfm**

```
inherited FRAMElistContacts: TFRAMElistContacts
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
```
<!-- tabs:end -->

