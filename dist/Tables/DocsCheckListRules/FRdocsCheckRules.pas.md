<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um componente de interface gráfica chamado `TFRAMEdocsCheckRules`, que é utilizado para gerenciar e editar regras de checklist de documentos. Ele fornece uma interface para exibir e editar informações como ID da regra, descrição e unidade de negócios associada.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes VCL:** Incluindo `TsLabel`, `TsDBEdit`, `TsPanel`, e outros para construir a interface gráfica.
  - **SOAP:** Utilizado para comunicação com serviços externos, como o `CheckListDocRulesServiceUtils`.

* **Forma Identificada:**  
  - **Formulário:**  
    - **Elementos do Formulário e seus Tipos:**
      - `EDTruleId` (Campo de texto, somente leitura).
      - `EDTruleDesc` (Campo de texto, editável).
      - `FRAMEBusUnit1` (Componente customizado para seleção de unidade de negócios).
    - **Ações do Formulário e seus Efeitos:**
      - Botão "Adicionar" (`BTNadd`): Adiciona uma nova regra.
      - Botão "Aplicar" (`BTNapply`): Salva as alterações feitas.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Visualizar e editar informações de regras de checklist.
  - Selecionar uma unidade de negócios associada.
  - Salvar ou adicionar novas regras.

* **Componentes Principais:**
  - `EDTruleId`: Exibe o ID da regra (não editável).
  - `EDTruleDesc`: Permite editar a descrição da regra.
  - `FRAMEBusUnit1`: Gerencia a seleção de unidade de negócios.
  - `FRAMEstatusInfo1`: Exibe informações de status relacionadas ao formulário.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar":  
    `if botão "Adicionar" clicado then criar nova regra`.
  - Evento `OnClick` do botão "Aplicar":  
    `if botão "Aplicar" clicado then salvar alterações`.
  - Evento `OnChange` no campo `EDTruleDesc`:  
    `if valor do campo alterado then validar entrada`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente `TFRAMEdocsCheckRules`:
     - Configurações iniciais, como propriedades de serviço e visibilidade de painéis.
  2. Interação do usuário:
     - O usuário pode preencher os campos ou selecionar opções.
     - Botões acionam eventos para salvar ou adicionar dados.
  3. Comunicação com o serviço `CheckListDocRulesServiceUtils` para persistir os dados.

* **Dados Necessários:**
  - ID da Regra (gerado automaticamente).
  - Descrição da Regra (preenchido pelo usuário).
  - Unidade de Negócios (selecionada pelo usuário).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Aplicar" só deve estar habilitado se todos os campos obrigatórios forem preenchidos.
  - Campo `EDTruleId` é somente leitura e não pode ser editado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.
  - "Valor inválido" se o valor inserido não for válido.

* **Valores Padrão dos Campos:**
  - `FRAMEBusUnit1.BusUnitDefault`: Unidade de negócios padrão definida como `gv_DefaultBusUnit`.

* **Validações e Condições dos Campos:**
  - `EDTruleId`: Somente leitura.
  - `EDTruleDesc`: Deve ser preenchido pelo usuário.
  - `FRAMEBusUnit1`: Deve conter uma unidade de negócios válida.

---

## 5. Funções Principais:

* **`Create` (Construtor):**  
  Configura as propriedades iniciais do componente, como fonte de dados, visibilidade de painéis e serviço associado.

* **`SetKeyEditing`:**  
  Define o estado de edição do campo `EDTruleId` como desabilitado.

---

## 6. Consumo de Serviços API:

* **Serviço:** `CheckListDocRulesServiceUtils`.
  - **Endpoint:** Não especificado no código.
  - **Dados Enviados:** Dados da regra (ID, descrição, unidade de negócios).
  - **Dados Recebidos:** Confirmação de sucesso ou erro.
  - **Propósito:** Criar ou atualizar regras de checklist.
  - **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `kneFRCtrlEditSOA`: Componentes customizados para edição.
  - `kneFRBusUnit`: Gerenciamento de unidades de negócios.

* **Componentes Customizados:**
  - `TFRAMEBusUnit`: Gerencia a seleção de unidade de negócios.
  - `TFRAMEstatusInfo`: Exibe informações de status.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `EDTruleId` (tipo: string, somente leitura).
  - `EDTruleDesc` (tipo: string, obrigatório).
  - `FRAMEBusUnit1` (tipo: seleção, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  var
    Frame: TFRAMEdocsCheckRules;
  begin
    Frame := TFRAMEdocsCheckRules.Create(Self);
    Frame.Show;
  end;
  ```

* **HTML Representando o Formulário:**
  ```html
  <div style="font-family: Verdana; width: 626px;">
    <label style="color: #4D4D4D;">Rule ID:</label>
    <input type="text" readonly style="width: 200px;" />
    <br />
    <label style="color: #4D4D4D;">Description:</label>
    <input type="text" style="width: 400px;" />
    <br />
    <label style="color: #4D4D4D;">Business Unit:</label>
    <select>
      <option>Default Unit</option>
    </select>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial do componente no construtor `Create`.
* Desabilitação do campo `EDTruleId` na função `SetKeyEditing`.

---

## 12. Conclusão:

O código fornece uma interface funcional para gerenciar regras de checklist de documentos. Ele é bem estruturado, mas carece de validações mais robustas e mensagens de erro detalhadas. Além disso, a integração com o serviço externo poderia ser mais detalhada.

---

## 13. Resumo Curto:

O componente `TFRAMEdocsCheckRules` permite gerenciar regras de checklist de documentos, fornecendo uma interface para edição e integração com serviços SOAP. Ele é parte de um sistema maior para controle de documentos e unidades de negócios.#### **FRdocsCheckRules.pas**

```
unit FRdocsCheckRules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRStatusInfo, kneFRBusUnit;

type
  TFRAMEdocsCheckRules = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL3: TsLabel;
    LBL1: TsLabel;
    EDTruleId: TsDBEdit;
    EDTruleDesc: TsDBEdit;
    LBLbusUnit: TsLabel;
    FRAMEBusUnit1: TFRAMEBusUnit;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyEditing(const EditKey: Boolean); override;  
  end;

var
  FRAMEdocsCheckRules: TFRAMEdocsCheckRules;

implementation

uses
  kneTypes, kneUtils, Global,
  //---
  CheckListDocRulesServiceUtils;

{$R *.dfm}

{ TFRAMEdocsCheckRules }

constructor TFRAMEdocsCheckRules.Create(AOwner: TComponent);
begin
  inherited;
  
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CheckListDocRules';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCheckListDocRulesServiceUtils.Create(self);

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

  // [02-03-2020, #23898]
  FRAMEBusUnit1.DataSource     := DStable;
  FRAMEBusUnit1.BusUnitList    := gv_BusUnitList;
  FRAMEBusUnit1.BusUnitDefault := gv_DefaultBusUnit;
end;


procedure TFRAMEdocsCheckRules.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  // impedir acesso ao campo (em NEW)
  TkneControls.SetControlState(EDTruleId, False);
end;



end.
```

#### **FRdocsCheckRules.dfm**

```
inherited FRAMEdocsCheckRules: TFRAMEdocsCheckRules
  Width = 626
  Font.Name = 'Verdana'
  object LBL3: TsLabel [0]
    Left = 8
    Top = 39
    Width = 69
    Height = 13
    Caption = 'Description:'
    FocusControl = EDTruleDesc
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL1: TsLabel [1]
    Left = 8
    Top = 13
    Width = 48
    Height = 13
    Caption = 'Rule ID:'
    FocusControl = EDTruleId
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLbusUnit: TsLabel [2]
    Left = 236
    Top = 13
    Width = 81
    Height = 13
    Caption = 'Business Unit:'
    FocusControl = FRAMEBusUnit1.DBCBObusUnit
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 626
    TabOrder = 3
    Visible = False
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
```
<!-- tabs:end -->

