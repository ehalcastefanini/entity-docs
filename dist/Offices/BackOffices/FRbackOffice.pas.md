<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRbackOffice` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRbackOffice` code unit defines a form (`TFRAMEbackOffice`) that serves as a user interface for managing back-office-related data. It provides fields for entering and displaying back-office descriptions, identifiers, and selecting a back-office assistant. The form integrates with data sources and utility services to facilitate data management and interaction with external systems.

### Technologies Used:
- **Delphi Framework**: Used for creating the form and managing its components.
- **SOAP Services**: Used for interacting with external services (`BackOfficeServiceUtils` and `BoAssistServiceUtils`).
- **Database Components**: Includes `TsDBEdit` for database-bound fields and `TDataSource` for data binding.
- **Custom Components**: Includes `TsLabel`, `TsDBEdit`, and `TFRAMEFindEditSOA` for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTdescrip` (Text Input, Database-bound): For entering the back-office description.
  - `EDTbackoffice` (Text Input, Database-bound): For entering the back-office identifier.
  - `FRAMEFindBOAssistant` (Custom Component): For selecting a back-office assistant.
  - `LBLname`, `LBLbackOffice`, `LBLGeneralManager` (Labels): For displaying field descriptions.
- **Form Actions and Effects**:
  - Data entry and validation for back-office details.
  - Integration with external services for assistant selection.

---

## 2. Functionality Description:

### User/Software Actions:
- Enter back-office details (description and identifier).
- Select a back-office assistant using a custom search dialog.
- Interact with external services for data retrieval and validation.

### Main Components:
- **Labels (`TsLabel`)**: Display field descriptions.
- **Database-bound Fields (`TsDBEdit`)**: Allow users to input and bind data to the database.
- **Custom Search Component (`TFRAMEFindEditSOA`)**: Provides a dialog for selecting a back-office assistant.
- **Status Info Frame (`TFRAMEstatusInfo`)**: Displays status information related to the data source.

### Pseudo-code for Actions and Events:
- **On Form Initialization**:
  - `if form created then initialize properties and services`.
- **On Assistant Selection**:
  - `if assistant selected then update field values`.
- **On Data Change**:
  - `if data field value changed then validate and update data source`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created, and its properties are initialized in the `Create` constructor.
   - Data source and service utilities are configured.
   - The assistant selection component (`FRAMEFindBOAssistant`) is set up.
2. **User Interaction**:
   - Users input data into the fields (`EDTdescrip`, `EDTbackoffice`).
   - Users select a back-office assistant using the search dialog.
3. **Service Interaction**:
   - External services (`BackOfficeServiceUtils`, `BoAssistServiceUtils`) are used for data validation and retrieval.

### Required Data:
- Back-office identifier (`backoffice`).
- Back-office description (`boDescrip`).
- Selected assistant details (`respons`, `respName`).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Assistant Selection**:
  - Preconditions: The data source must be active.
  - Action: Opens a dialog for selecting an assistant.
- **Data Entry**:
  - Preconditions: Fields must be enabled and bound to a data source.
  - Action: Updates the database with entered values.

### Available Filters:
- Assistant selection dialog filters by:
  - Code (`boAssist`).
  - Description (`name`).

### Error Messages:
- "Data source not active" if the data source is not initialized.
- "Invalid assistant selection" if no valid assistant is selected.

### Default Field Values:
- `EDTdescrip`: Empty by default.
- `EDTbackoffice`: Empty by default.

### Field Validation and Conditions:
- `EDTdescrip`: Must be uppercase.
- `EDTbackoffice`: Must be uppercase and linked to the database.

---

## 5. Main Functions:

### Functions:
1. **`Create` Constructor**:
   - Initializes the form and its components.
   - Configures data sources and services.
2. **`m_SetFindBoAssist`**:
   - Configures the assistant selection component (`FRAMEFindBOAssistant`).
   - Sets up data binding and dialog options.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `BackOfficeServiceUtils`
   - **Endpoint**: Not explicitly defined in the code.
   - **Purpose**: Provides data management for back-office entities.
2. **Service Name**: `BoAssistServiceUtils`
   - **Endpoint**: Not explicitly defined in the code.
   - **Purpose**: Provides data for assistant selection.

---

## 7. Conditional Fields (Form Logic):

- **Field**: Assistant Selection (`FRAMEFindBOAssistant`).
- **Condition**: Visible and functional only when the data source is active.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based service communication.
- **DBClient**: For database operations.

### Custom Components:
- **`TFRAMEFindEditSOA`**: Custom component for assistant selection.
- **`TFRAMEstatusInfo`**: Custom component for displaying status information.

---

## 9. Fields and Validations Listing:

### Fields:
1. **`EDTdescrip`**:
   - Type: String.
   - Required: Yes.
   - Validation: Uppercase.
   - Database Field: `boDescrip`.
2. **`EDTbackoffice`**:
   - Type: String.
   - Required: Yes.
   - Validation: Uppercase.
   - Database Field: `backoffice`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Form Initialization] --> [Configure Data Sources and Services] --> [User Interaction] --> [Service Interaction]
```

### Sequence Diagram:
```plaintext
User --> Form: Input Data
User --> Form: Select Assistant
Form --> Service: Validate/Fetch Data
Service --> Form: Return Data
```

### Code Snippets:
```delphi
procedure TFRAMEbackOffice.m_SetFindBoAssist;
begin
  with FRAMEFindBOAssistant do
  begin
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'respons';
    EditSettings.FieldNameForDesc := 'respName';
    FindDialog.Caption := 'Back Office Assistant Selection';
    FindDialog.ProviderService := TBoAssistServiceUtils.Create(FindDialog);
  end;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 724px;">
  <label style="position: absolute; top: 48px; left: 16px;">Description:</label>
  <input type="text" style="position: absolute; top: 42px; left: 86px; width: 553px;" />
  <label style="position: absolute; top: 16px; left: 16px;">Back Office:</label>
  <input type="text" style="position: absolute; top: 10px; left: 86px; width: 121px;" />
  <label style="position: absolute; top: 80px; left: 16px;">CSA Manager:</label>
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization**:
  - `// SET DAS PROPRIEDADES DA FRAME`: Configures frame properties.
- **Assistant Selection**:
  - `// configura�ao da Find Edit`: Configures the assistant selection component.

---

## 12. Conclusion:

The `FRbackOffice` code unit provides a robust form for managing back-office data, integrating with external services for assistant selection. While it is well-structured, the lack of explicit error handling and endpoint definitions could be improved.

---

## 13. Short Summary:

The `FRbackOffice` form facilitates back-office data management with database integration and assistant selection via external services, ensuring efficient data handling and user interaction.#### **FRbackOffice.pas**

```
unit FRbackOffice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRStatusInfo, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRFindEditSOA;

type
  TFRAMEbackOffice = class(TFRAMEBaseCtrlEditSOA)
    LBLname: TsLabel;
    LBLbackOffice: TsLabel;
    EDTdescrip: TsDBEdit;
    EDTbackoffice: TsDBEdit;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBLGeneralManager: TsLabel;
    FRAMEFindBOAssistant: TFRAMEFindEditSOA;
  private
    procedure m_SetFindBoAssist;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEbackOffice: TFRAMEbackOffice;

implementation

uses
  kneUtils, kneTypes,
  BackOfficeServiceUtils, BoAssistServiceUtils;

{$R *.dfm}

constructor TFRAMEbackOffice.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'BackOffice';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TBackOfficeServiceUtils.Create(self);

  //Configura��o do findEdit 
  m_SetFindBoAssist;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
  ServiceParams.ShowInactives := True;
end;

procedure TFRAMEbackOffice.m_SetFindBoAssist;
begin
  with FRAMEFindBOAssistant do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'respons';
    EditSettings.FieldNameForDesc := 'respName';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'boAssist';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('name');

    FindDialog.Caption := 'Back Office Assistant Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TBoAssistServiceUtils.Create(FindDialog);
  end;
end;

end.
```

#### **FRbackOffice.dfm**

```
inherited FRAMEbackOffice: TFRAMEbackOffice
  Width = 724
  object LBLname: TsLabel [0]
    Left = 16
    Top = 48
    Width = 57
    Height = 13
    Caption = 'Description:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLbackOffice: TsLabel [1]
    Left = 16
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Back Office:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLGeneralManager: TsLabel [2]
    Left = 16
    Top = 80
    Width = 69
    Height = 13
    BiDiMode = bdLeftToRight
    Caption = 'CSA Manager:'
    FocusControl = FRAMEFindBOAssistant.DBE
    ParentBiDiMode = False
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 724
    TabOrder = 4
    Visible = False
  end
  object EDTdescrip: TsDBEdit [4]
    Left = 86
    Top = 42
    Width = 553
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'boDescrip'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTbackoffice: TsDBEdit [5]
    Left = 86
    Top = 10
    Width = 121
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'backoffice'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
```
<!-- tabs:end -->

